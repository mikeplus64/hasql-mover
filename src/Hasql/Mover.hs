{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Hasql.Mover (
  Migration (..),
  SomeMigration (..),

  -- * Declaration
  declareMigration,

  -- * Checking and running migrations

  -- ** Main functions
  hasqlMover,
  performMigrations,

  -- *** Options
  MigrationCli (..),
  MigrationCmd (..),

  -- *** Results
  MigrationError (..),

  -- *** Checked migrations
  UpMigration (..),
  PendingMigration (..),
  DivergentMigration (..),

  -- ** Settings
  MigrationDB (..),
  migrationDBFromSettings,

  -- ** Integrating into an existing "main"
  hasqlMoverOpts,
) where

import Control.Exception qualified as E
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Control.Monad.Trans.State.Strict (State, StateT (..), execState, execStateT, gets, modify', put)
import Data.Char (isSpace)
import Data.Proxy (Proxy (..))
import Data.SOP.Constraint (All)
import Data.SOP.NP (NP (..), cpure_NP, ctraverse__NP, traverse__NP)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Typeable (Typeable, cast)
import Data.Void (Void)
import Hasql.Connection qualified as Sql
import Hasql.Session qualified as Sql
import Hasql.TH qualified as Sql
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Tx
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Options.Applicative qualified as O
import Prettyprinter ((<+>))
import Prettyprinter qualified as R
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, colorDull, putDoc)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
-- Checked migrations -- these have been checked against the current database
-- status

data PendingMigration = forall m. (Migration m) => PendingMigration {migration :: m}
data UpMigration = forall m. (Migration m) => UpMigration {migration :: m, executedAt :: UTCTime}
data DivergentMigration = forall m. (Migration m) => DivergentMigration {migration :: m, oldUp, oldDown :: Text, executedAt :: UTCTime}
data UnknownMigration m = (Migration m) => UnknownMigration m
data SomeMigration where SomeMigration :: (Migration m) => m -> SomeMigration

type Doc = R.Doc AnsiStyle

prettyPending :: PendingMigration -> Doc
prettyPending PendingMigration {migration} =
  R.vsep
    [ "Pending " <+> R.viaShow migration <+> R.line
    , R.annotate (colorDull Green) "[up]" <+> R.line
    , R.pretty (up migration) <+> R.line
    ]

prettyUp :: UpMigration -> Doc
prettyUp UpMigration {migration, executedAt} =
  R.vsep
    [ "Up " <+> R.viaShow migration <+> "executed at" <+> R.viaShow executedAt <+> R.line
    , R.annotate (colorDull Green) "[up]" <+> R.line
    , R.pretty (up migration) <+> R.line
    ]

prettyDivergent :: DivergentMigration -> Doc
prettyDivergent DivergentMigration {migration, oldUp, executedAt} =
  R.vsep
    [ "Divergent " <+> R.viaShow migration <+> "executed at" <+> R.viaShow executedAt <+> R.line
    , R.annotate (colorDull Green) "[up/new]" <+> R.line
    , R.pretty (up migration) <+> R.line
    , R.annotate (colorDull Green) "[up/old]" <+> R.line
    , R.pretty oldUp <+> R.line
    ]

prettyRollback :: (Migration m) => Rollback m -> Doc
prettyRollback (Rollback m) =
  R.vsep
    [ "Rollback of " <+> R.viaShow m <+> R.line
    , R.annotate (colorDull Green) "[down]" <+> R.line
    , R.pretty (down m) <+> R.line
    ]

instance Show PendingMigration where
  showsPrec p (PendingMigration m) = showParen (p > 10) (showString "PendingMigration " . showsPrec 11 m)

instance Show UpMigration where
  showsPrec p (UpMigration m e) = showParen (p > 10) (showString "UpMigration " . showsPrec 11 e . showString " " . showsPrec 11 m)

data CheckedMigrations names = CheckedMigrations
  { ups :: [UpMigration]
  , divergents :: [DivergentMigration]
  , pendings :: [PendingMigration]
  }

-- | A mapping from a singleton migration name to its up and down SQL
class (Typeable a, Show a) => Migration a where
  -- | The name for this migration
  migration :: a

  -- | How to run this migration
  up :: a -> Text

  -- | How to rollback this migration
  down :: a -> Text

migrationName :: (Migration a) => a -> Text
migrationName = Text.pack . show

--------------------------------------------------------------------------------
-- The base migration

-- | Sets up necessary tables for hasql-mover
data BaseMigration = BaseMigration
  deriving stock (Show, Read, Eq, Ord)

instance Migration BaseMigration where
  migration = BaseMigration
  up _ =
    "CREATE TABLE hasql_mover_migration (\n\
    \  id serial NOT NULL,\n\
    \  name text NOT NULL,\n\
    \  up text NOT NULL,\n\
    \  down text NOT NULL,\n\
    \  executed_at timestamptz NOT NULL DEFAULT now()\n\
    \)"
  down _ =
    "DROP TABLE hasql_mover_migration CASCADE"

newtype Rollback m = Rollback m
  deriving stock (Show, Read)

instance (Migration m) => Migration (Rollback m) where
  migration = Rollback migration
  up (Rollback m) = down m
  down (Rollback m) = up m

--------------------------------------------------------------------------------

type CheckM = StateT CheckState Tx.Transaction

data CheckState = CheckState
  { ups :: [UpMigration]
  , divergents :: [DivergentMigration]
  , pendings :: [PendingMigration]
  , haveBaseTable :: Bool
  }

checkMigrations :: forall migrations. (All Migration migrations) => Sql.Session (CheckedMigrations (BaseMigration : migrations))
checkMigrations =
  runCheckState do
    checkBaseMigration
    checkOthers
  where
    runCheckState :: CheckM () -> Sql.Session (CheckedMigrations (BaseMigration : migrations))
    runCheckState s = Tx.transaction Tx.Serializable Tx.Read do
      CheckState {ups, divergents, pendings} <- execStateT s (CheckState [] [] [] True)
      pure
        CheckedMigrations
          { ups = reverse ups
          , divergents = reverse divergents
          , pendings = reverse pendings
          }

    checkBaseMigration :: CheckM ()
    checkBaseMigration = do
      haveBaseTable <- lift $ Tx.statement () [Sql.singletonStatement|SELECT (to_regclass('hasql_mover_migration') IS NOT NULL)::boolean|]
      if haveBaseTable
        then checkMigration BaseMigration
        else do
          modify' \s -> s {haveBaseTable = False}
          addPending BaseMigration

    checkOthers :: CheckM ()
    checkOthers =
      traverse__NP
        (\(UnknownMigration m) -> checkMigration m)
        (cpure_NP (Proxy @Migration) (UnknownMigration migration) :: NP UnknownMigration migrations)

    checkMigration :: (Migration m) => m -> CheckM ()
    checkMigration m = do
      canContinue <- gets \CheckState {haveBaseTable, divergents} -> haveBaseTable && null divergents
      if canContinue
        then do
          r <-
            lift $
              Tx.statement
                (migrationName m)
                [Sql.maybeStatement|
                  SELECT up::text, down::text, executed_at::timestamptz
                  FROM hasql_mover_migration
                  WHERE name = $1::text
                |]
          case r of
            Just (oldUp, oldDown, executedAt)
              | oldUp == up m -> addUp m executedAt
              | otherwise -> addDivergent m executedAt oldUp oldDown
            Nothing -> addPending m
        else addPending m

    addDivergent :: (Migration m) => m -> UTCTime -> Text -> Text -> CheckM ()
    addDivergent migration executedAt oldUp oldDown =
      modify' \CheckState {..} -> CheckState {divergents = DivergentMigration {migration, ..} : divergents, ..}

    addPending :: (Migration m) => m -> CheckM ()
    addPending migration =
      modify' \CheckState {..} -> CheckState {pendings = PendingMigration {migration, ..} : pendings, ..}

    addUp :: (Migration m) => m -> UTCTime -> CheckM ()
    addUp migration executedAt =
      modify' \CheckState {..} -> CheckState {ups = UpMigration {migration, ..} : ups, ..}

--------------------------------------------------------------------------------
-- Performing migrations

-- | Encapsulates a way to run a hasql session; it could be through a pool, or
-- through a connection directly.
data MigrationDB = forall db.
  MigrationDB
  { acquire :: IO (Either Sql.ConnectionError db)
  , release :: db -> IO ()
  , run :: forall a. Sql.Session a -> db -> IO (Either Sql.QueryError a)
  }

-- | Create a 'MigrationDB' from a hasql settings - a PostgreSQL connection
-- string as of writing
migrationDBFromSettings :: Sql.Settings -> MigrationDB
migrationDBFromSettings connstr =
  MigrationDB
    { acquire = Sql.acquire connstr
    , release = Sql.release
    , run = Sql.run
    }

-- | Options to supply to 'hasqlMover': a database connection and what migration
-- command to run
data MigrationCli = MigrationCli
  { db :: MigrationDB
  , cmd :: MigrationCmd
  }

-- | A command for 'hasqlMover'
data MigrationCmd
  = -- | Run pending migrations
    MigrateUp
  | -- | Rollback a migration
    MigrateDown
      { undoDivergents :: Bool
      -- ^ Are we allowed to undo a divergent migration? Default: No
      , divergentUseOldDown :: Bool
      -- ^ For a divergent migration, use the previous "down" SQL text, or the new one? Default: New down
      }
  | -- | Print the current status
    MigrateStatus
  | -- | Force the 'up' of a migration to run, regardless of its status or position in the migrations list
    MigrateForceUp Text
  | -- | Force the 'down' of a migration to run, regardless of its status or position in the migrations list
    MigrateForceDown Text

-- | optparse-applicative options for hasql-mover; use 'hasqlMover' to then run the parsed options
hasqlMoverOpts :: O.Parser MigrationCli
hasqlMoverOpts =
  MigrationCli
    <$> (migrationDBFromSettings . Text.encodeUtf8 . Text.pack <$> O.strOption (O.long "db" <> O.metavar "DB"))
    <*> O.subparser
      ( mconcat
          [ O.command "up" (O.info (pure MigrateUp) (O.progDesc "Perform any pending migrations"))
          , O.command "down" (O.info migrateDown (O.progDesc "Rollback the last migration"))
          , O.command "force-up" (O.info migrateForceUp (O.progDesc "Run a given up migration"))
          , O.command "force-down" (O.info migrateForceDown (O.progDesc "Run a given down migration"))
          , O.command "status" (O.info (pure MigrateStatus) (O.progDesc "Check current status"))
          ]
      )
  where
    migrateForceUp = MigrateForceUp <$> O.strArgument mempty
    migrateForceDown = MigrateForceDown <$> O.strArgument mempty
    migrateDown =
      MigrateDown
        <$> O.switch (O.long "undo-diverging" <> O.short 'u' <> O.help "Can we undo a diverging migration?")
        <*> O.switch (O.long "divergent-down-from-old" <> O.short 'o' <> O.help "Use the 'down' definition for a divergent migration from its original definition, when it was initially ran")

-- | Main function for running hasql-mover migrations
--
-- Example usage:
--
-- @
-- [declareMigration|
-- name = V0
--
-- [up]
-- CREATE TABLE foo ();
--
-- [down]
-- DROP TABLE foo CASCADE;
-- |]
--
-- type Migrations = '[V0]
--
-- main :: IO ()
-- main = hasqlMoverMain @Migrations
-- @
hasqlMover :: forall ms. (All Migration ms) => IO ()
hasqlMover = do
  cli <-
    O.execParser
      ( O.info
          (hasqlMoverOpts O.<**> O.helper)
          (O.fullDesc <> O.progDesc "Perform or check hasql-mover migrations")
      )
  result <- performMigrations @ms cli
  case result of
    Right () -> putStrLn "Done"
    Left err -> putDoc (prettyMigrationError err <+> R.softline)

data MigrationError
  = MigrationCheckError !Sql.QueryError
  | MigrationUpError !PendingMigration !Sql.QueryError
  | MigrationDownError !UpMigration !Sql.QueryError
  | MigrationForceUpError !SomeMigration !Sql.QueryError
  | MigrationForceDownError !SomeMigration !Sql.QueryError
  | MigrationDivergentDownError !DivergentMigration !Sql.QueryError
  | MigrationConnectError !Sql.ConnectionError
  | MigrationNothingToRollback
  | MigrationGotDivergents
  | MigrationException !E.SomeException
  | MigrationNotFound !Text

prettyMigrationError :: MigrationError -> Doc
prettyMigrationError = \case
  MigrationCheckError qe -> "Check error" <+> prettyQueryError qe
  MigrationUpError pending qe -> "Up error" <+> prettyPending pending <+> prettyQueryError qe
  MigrationDownError up qe -> "Down error" <+> prettyUp up <+> prettyQueryError qe
  MigrationForceUpError (SomeMigration m) qe -> "Forced up error" <+> R.viaShow m <+> prettyQueryError qe
  MigrationForceDownError (SomeMigration m) qe -> "Forced down error" <+> R.viaShow m <+> prettyQueryError qe
  MigrationDivergentDownError up qe -> "Divergent down error" <+> prettyDivergent up <+> prettyQueryError qe
  MigrationConnectError connerr -> "Connection error" <+> R.viaShow connerr
  MigrationNothingToRollback -> "Nothing to roll back"
  MigrationGotDivergents -> "Divergent migrations"
  MigrationException se -> R.viaShow se
  MigrationNotFound name -> "Migration not found:" <+> R.viaShow name
  where
    prettyQueryError (Sql.QueryError bs params cmderr) =
      R.vsep
        [ "QueryError for  " <+> R.align (R.vsep (map R.pretty (Text.lines (Text.decodeUtf8 bs))))
        , " - Params: " <+> R.align (R.list (map R.pretty params))
        , " - Command Error: " <+> R.align case cmderr of
            Sql.ClientError mc -> "ClientError " <+> foldMap (R.pretty . Text.decodeUtf8) mc
            Sql.ResultError re -> "ResultError " <+> prettyResultError re
        ]
    prettyResultError = \case
      Sql.ServerError code message details hint pos ->
        R.vsep
          [ "ServerError " <+> R.viaShow code <+> ": " <+> R.pretty (Text.decodeUtf8 message)
          , foldMap ((<+>) "Details: " . R.align . R.pretty . Text.decodeUtf8) details
          , foldMap ((<+>) "Hint: " . R.align . R.pretty . Text.decodeUtf8) hint
          , foldMap ((<+>) "Position: " . R.align . R.pretty) pos
          ]
      Sql.UnexpectedResult err -> R.pretty err
      err -> R.viaShow err

-- | Perform the migrations according to some 'MigrationCli'
performMigrations
  :: forall migrations
   . (All Migration migrations)
  => MigrationCli
  -> IO (Either MigrationError ())
performMigrations MigrationCli {db = MigrationDB {acquire, release, run}, cmd} = runResourceT $ runExceptT do
  (_releaseKey, mdb) <- allocate acquire \case
    Left _ -> pure ()
    Right db -> release db
  db <- errBy MigrationConnectError (pure mdb)
  let
    check = errBy MigrationCheckError (runSession (checkMigrations @migrations))

    runSession :: (MonadIO m) => Sql.Session a -> m (Either Sql.QueryError a)
    runSession s = liftIO (run s db)

    runPending :: (Migration m) => m -> IO (Either Sql.QueryError UTCTime)
    runPending m = do
      putDoc (prettyPending (PendingMigration m))
      runSession $ Tx.transaction Tx.Serializable Tx.Write do
        Tx.sql $ Text.encodeUtf8 $ up m
        Tx.statement
          (migrationName m, up m, down m)
          [Sql.singletonStatement|
              INSERT INTO hasql_mover_migration (name, up, down) VALUES($1::text, $2::text, $3::text)
              RETURNING executed_at::timestamptz
            |]

    runRollback :: (Migration m) => m -> Text -> IO (Either Sql.QueryError ())
    runRollback m downSql = do
      putDoc (prettyRollback (Rollback m))
      runSession $ Tx.transaction Tx.Serializable Tx.Write do
        Tx.sql $ Text.encodeUtf8 downSql
        case cast m of
          Just BaseMigration -> pure ()
          Nothing -> Tx.statement (migrationName m) [Sql.resultlessStatement|DELETE FROM hasql_mover_migration WHERE name = ($1::text)|]

  checked@CheckedMigrations {ups, divergents, pendings} <- check

  let
    findMigrationByName :: Text -> Maybe SomeMigration
    findMigrationByName name =
      (`execState` Nothing) do
        ctraverse__NP (Proxy @Migration) findIt allMigrations
      where
        findIt :: (Migration m) => UnknownMigration m -> State (Maybe SomeMigration) ()
        findIt (UnknownMigration m) =
          when
            (Text.pack (show m) == name)
            (put (Just (SomeMigration m)))

        allMigrations = cpure_NP (Proxy @Migration) (UnknownMigration migration) :: NP UnknownMigration migrations

  case cmd of
    -- Status
    MigrateStatus -> liftIO (putDoc (ppStatus "Current migrations status" checked))
    -- Up
    MigrateUp
      | null divergents -> do
          forM_ pendings \p@PendingMigration {migration} -> do
            _ <- wrapQuery (MigrationUpError p) (runPending migration)
            liftIO . putDoc . ppStatus "New migrations status" =<< check
      | otherwise -> throwE MigrationGotDivergents
    -- Down
    MigrateDown {undoDivergents, divergentUseOldDown}
      | null ups && (null divergents && not undoDivergents) -> throwE MigrationNothingToRollback
      | undoDivergents && not (null divergents)
      , u@DivergentMigration {migration, oldDown} <- last divergents ->
          wrapQuery
            (MigrationDivergentDownError u)
            (runRollback migration (if divergentUseOldDown then oldDown else down migration))
      | not (null divergents) ->
          throwE MigrationGotDivergents
      | u@UpMigration {migration} <- last ups -> do
          wrapQuery (MigrationDownError u) (runRollback migration (down migration))
          liftIO . putDoc . ppStatus "New migrations status" =<< check
    -- Forcing down
    MigrateForceDown nameText | Just (SomeMigration m) <- findMigrationByName nameText -> do
      wrapQuery (MigrationForceDownError (SomeMigration m)) (runRollback m (down m))
    MigrateForceDown nameText -> throwE (MigrationNotFound nameText)
    -- Forcing up
    MigrateForceUp nameText | Just (SomeMigration m) <- findMigrationByName nameText -> void do
      wrapQuery (MigrationForceUpError (SomeMigration m)) (runPending m)
    MigrateForceUp nameText -> throwE (MigrationNotFound nameText)
  where
    ppStatus :: Text -> CheckedMigrations m -> Doc
    ppStatus title CheckedMigrations {ups, divergents, pendings} =
      R.vsep
        [ R.pretty title
        , R.vsep $
            concat @[]
              [ map ppUp ups
              , map ppDivergent divergents
              , map ppPending pendings
              ]
        , R.softline
        ]

    ppUp UpMigration {migration, executedAt} =
      R.annotate (color Green) $ R.hsep ["[ UP ", R.viaShow executedAt, " ]", R.align (R.viaShow migration)]
    ppDivergent DivergentMigration {migration, executedAt} =
      R.annotate (color Red) $ R.hsep ["[ DIVERGENT ", R.viaShow executedAt, " ]", R.align (R.viaShow migration)]
    ppPending PendingMigration {migration} =
      R.annotate (colorDull White) $ R.hsep ["[ PENDING ]", R.align (R.viaShow migration)]

    errBy f a = withExceptT f (ExceptT a)
    wrapQuery f p = do
      r <- liftIO (E.try @E.SomeException p)
      case r of
        Left se -> throwE (MigrationException se)
        Right (Left e) -> throwE (f e)
        Right (Right a) -> pure a

--------------------------------------------------------------------------------
-- Declaring migrations

-- | Declare a migration with a nice syntax.
--
-- @
-- [declareMigration|
-- name = AddFoo
--
-- [up]
-- CREATE TABLE foo();
--
-- [down]
-- DROP TABLE foo();
-- |]
--
-- type Migrations = '[BaseMigration, AddFoo]
--
-- @
declareMigration :: TH.QuasiQuoter
declareMigration =
  TH.QuasiQuoter
    { quoteExp = undefined
    , quoteType = undefined
    , quotePat = undefined
    , quoteDec = \s ->
        case M.parse parseMigrationDesc "hasql-mover" (Text.pack s) of
          Left err -> error (M.errorBundlePretty err)
          Right MigrationDesc {name, up, down} -> do
            qtype <- TH.newName (Text.unpack name)
            qconstr <- TH.newName (Text.unpack name)
            dec <- TH.dataD (pure []) qtype [] Nothing [TH.normalC qconstr []] [TH.derivClause (Just TH.StockStrategy) [[t|Show|]]]
            inst <-
              [d|
                instance Migration $(TH.conT qtype) where
                  migration = $(TH.conE qconstr)
                  up _ = $(TH.lift (Text.unpack up))
                  down _ = $(TH.lift (Text.unpack down))
                |]
            pure (dec : inst)
    }

-- Parsing for declareMigration
----------------------------------------

data MigrationDesc = MigrationDesc {name, up, down :: Text}
  deriving stock (Show)

type P = M.Parsec Void Text

parseMigrationDesc :: P MigrationDesc
parseMigrationDesc = do
  _ <- M.takeWhileP Nothing isSpace
  name <- symbol "name" >> symbol "=" >> parseName <* vspace
  header "up"
  up <- "up sql" `M.label` M.manyTill M.anySingle (M.try (M.newline >> header "down"))
  down <- "down sql" `M.label` M.takeRest
  pure
    MigrationDesc
      { name
      , up = Text.strip (Text.pack up)
      , down = Text.strip down
      }

vspace :: P ()
vspace = void (M.takeWhile1P Nothing (\c -> c == '\n' || c == '\r'))

header :: Text -> P ()
header t = void (M.label ("section header " <> Text.unpack h) (M.chunk h))
  where
    h = Text.concat ["[", t, "]\n"]

symbol :: Text -> P ()
symbol = void . L.symbol M.hspace

lexeme :: P a -> P a
lexeme = L.lexeme M.hspace

parseName :: P Text
parseName =
  M.label "name of the migration" $
    lexeme
      ( Text.pack <$> do
          c1 <- M.upperChar
          cs <- M.many M.alphaNumChar
          pure (c1 : cs)
      )
