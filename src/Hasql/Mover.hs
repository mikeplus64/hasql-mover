module Hasql.Mover (
  Migration (..),
  SomeMigration (..),

  -- * Declaration
  declareMigration,
  declareMigrationFromDirectory,
  migrationFromDirectory,

  -- * Checking and running migrations

  -- ** Main functions
  hasqlMover,
  hasqlMoverWith,
  performMigrations,
  PerformMigrationOpts (..),
  defaultPerformMigrationOpts,

  -- ** Checking
  CheckedMigrations (..),
  checkMigrations,

  -- *** Options
  MigrationCli (..),
  MigrationCmd (..),

  -- *** Results
  MigrationError (..),

  -- *** Checked migrations
  UpMigration (..),
  PendingMigration (..),
  DivergentMigration (..),

  -- ** Utilities
  RenamedMigration (..),
  SMigration (..),

  -- ** Settings
  MigrationDB (..),
  migrationDBFromSettings,

  -- ** Integrating into an existing "main"
  hasqlMoverCliOpts,
  hasqlMoverCliParserInfo,
) where

import Control.Exception qualified as E
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), mapExceptT, runExceptT, throwE, withExceptT)
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Control.Monad.Trans.State.Strict (State, StateT (..), execState, execStateT, gets, modify', put)
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.Proxy (Proxy (..))
import Data.SOP.Constraint (All)
import Data.SOP.NP (NP (..), cpure_NP, ctraverse__NP, traverse__NP)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format qualified as Time
import Data.Typeable (Typeable, cast)
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Hasql.Connection qualified as Sql
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting qualified as Sql
import Hasql.Connection.Setting.Connection qualified
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
import System.Directory (doesDirectoryExist, withCurrentDirectory)
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

instance Show SomeMigration where
  showsPrec _ (SomeMigration m) = showString (Text.unpack (migrationName m))

type Doc = R.Doc AnsiStyle

prettyPending :: PendingMigration -> Doc
prettyPending PendingMigration {migration} =
  R.vsep
    [ "Pending " <+> R.pretty (migrationName migration)
    , R.annotate (colorDull Green) "[up]"
    , R.pretty (up migration)
    , R.hardline
    ]

prettyUp :: UpMigration -> Doc
prettyUp UpMigration {migration, executedAt} =
  R.vsep
    [ "Up " <+> R.pretty (migrationName migration) <+> "executed at" <+> R.viaShow executedAt
    , R.annotate (colorDull Green) "[up]"
    , R.pretty (up migration)
    , R.hardline
    ]

prettyDivergent :: DivergentMigration -> Doc
prettyDivergent DivergentMigration {migration, oldUp, executedAt} =
  R.vsep
    [ "Divergent " <+> R.pretty (migrationName migration) <+> "executed at" <+> R.viaShow executedAt
    , R.annotate (colorDull Green) "[up/new]"
    , R.pretty (up migration)
    , R.hardline
    , R.annotate (colorDull Green) "[up/old]"
    , R.pretty oldUp
    , R.hardline
    ]

prettyRollback :: (Migration m) => Rollback m -> Doc
prettyRollback (Rollback m) =
  R.vsep
    [ "Rollback of" <+> R.pretty (migrationName m)
    , ""
    , R.annotate (colorDull Green) "[down]"
    , R.pretty (down m)
    , R.hardline
    ]

instance Show PendingMigration where
  showsPrec p (PendingMigration m) =
    showParen
      (p > 10)
      (showString "PendingMigration " . showString (Text.unpack (migrationName m)))

instance Show UpMigration where
  showsPrec p (UpMigration m e) =
    showParen
      (p > 10)
      (showString "UpMigration " . showsPrec 11 e . showString " " . showString (Text.unpack (migrationName m)))

instance Show DivergentMigration where
  showsPrec p (DivergentMigration m _ _ _) =
    showParen
      (p > 10)
      (showString "DivergentMigration " . showString (Text.unpack (migrationName m)))

data CheckedMigrations names = CheckedMigrations
  { ups :: [UpMigration]
  , divergents :: [DivergentMigration]
  , pendings :: [PendingMigration]
  }

-- | A mapping from a singleton migration name to its up and down SQL
class (Typeable a) => Migration a where
  -- | The name for this migration
  migration :: a

  migrationName :: a -> Text
  default migrationName :: (Show a) => a -> Text
  migrationName = Text.pack . show

  directory :: a -> Maybe FilePath
  directory _ = Nothing

  -- | How to run this migration
  up :: a -> Text

  -- | How to rollback this migration
  down :: a -> Text

--------------------------------------------------------------------------------
-- Utility migrations

newtype RenamedMigration (newName :: Symbol) m = RenamedMigration m
  deriving stock (Show)

instance (KnownSymbol newName, Migration m) => Migration (RenamedMigration newName m) where
  migration = RenamedMigration (migration @m)
  migrationName _ = Text.pack (symbolVal @newName Proxy)
  up (RenamedMigration m) = up m
  down (RenamedMigration m) = down m

data SMigration (name :: Symbol) (dir :: Maybe Symbol) (up :: Symbol) (down :: Symbol) = SMigration
  deriving stock (Show)

instance (KnownSymbol name, KnownSymbol up, KnownSymbol down, KnownMaybeSymbol dir, Typeable dir) => Migration (SMigration name dir up down) where
  migration = SMigration
  migrationName _ = Text.pack (symbolVal @name Proxy)
  directory _ = symbolMaybeVal (Proxy :: Proxy dir)
  up _ = Text.strip (Text.pack (symbolVal @up Proxy))
  down _ = Text.strip (Text.pack (symbolVal @down Proxy))

class KnownMaybeSymbol (k :: Maybe Symbol) where
  symbolMaybeVal :: Proxy k -> Maybe String

instance KnownMaybeSymbol 'Nothing where
  symbolMaybeVal _ = Nothing

instance (KnownSymbol dir) => KnownMaybeSymbol ('Just dir) where
  symbolMaybeVal _ = Just (symbolVal @dir Proxy)

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
  migrationName m = "Rollback " <> migrationName m
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

-- | Get the current status of @hasql-mover@ migrations
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
data MigrationDB
  = forall db.
  MigrationDB
  { acquire :: IO (Either Sql.ConnectionError db)
  , release :: db -> IO ()
  , run :: forall a. Sql.Session a -> db -> IO (Either Sql.SessionError a)
  }

-- | Create a 'MigrationDB' from a hasql settings - a PostgreSQL connection
-- string as of writing
migrationDBFromSettings :: [Sql.Setting] -> MigrationDB
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
    MigrateForceUp {migrationName :: Text}
  | -- | Force the 'down' of a migration to run, regardless of its status or position in the migrations list
    MigrateForceDown {migrationName :: Text}

-- | optparse-applicative options for hasql-mover; use 'hasqlMover' to then run the parsed options
hasqlMoverCliParserInfo :: O.ParserInfo MigrationCli
hasqlMoverCliParserInfo =
  O.info
    (hasqlMoverCliOpts O.<**> O.helper)
    (O.fullDesc <> O.progDesc "Perform or check hasql-mover migrations")

hasqlMoverCliOpts :: O.Parser MigrationCli
hasqlMoverCliOpts =
  MigrationCli
    <$> ( migrationDBFromSettings
            . pure
            . Hasql.Connection.Setting.connection
            . Hasql.Connection.Setting.Connection.string -- TODO this will break again soon
            . Text.pack
            <$> O.strOption (O.long "db" <> O.metavar "DB")
        )
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

data PerformMigrationOpts = PerformMigrationOpts
  { beforeUp, beforeDown :: forall m. (Migration m) => m -> IO (Either Text ())
  , afterUp, afterDown :: forall m. (Migration m) => m -> IO ()
  }

defaultPerformMigrationOpts :: PerformMigrationOpts
defaultPerformMigrationOpts =
  PerformMigrationOpts
    { beforeUp = const (pure (Right ()))
    , beforeDown = const (pure (Right ()))
    , afterUp = const (pure ())
    , afterDown = const (pure ())
    }

hasqlMoverWith :: forall ms. (All Migration ms) => MigrationCli -> PerformMigrationOpts -> IO ()
hasqlMoverWith cli opts = do
  result <- performMigrations @ms opts cli
  case result of
    Right () -> pure ()
    Left err -> putDoc (prettyMigrationError err <+> R.softline)

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
-- main = hasqlMover @Migrations
-- @
hasqlMover :: forall ms. (All Migration ms) => IO ()
hasqlMover = do
  cli <-
    O.execParser
      ( O.info
          (hasqlMoverCliOpts O.<**> O.helper)
          (O.fullDesc <> O.progDesc "Perform or check hasql-mover migrations")
      )
  hasqlMoverWith @ms cli defaultPerformMigrationOpts

data MigrationError
  = MigrationCheckError !Sql.SessionError
  | MigrationUpError !PendingMigration !Sql.SessionError
  | MigrationDownError !UpMigration !Sql.SessionError
  | MigrationForceUpError !SomeMigration !Sql.SessionError
  | MigrationForceDownError !SomeMigration !Sql.SessionError
  | MigrationDivergentDownError !DivergentMigration !Sql.SessionError
  | MigrationConnectError !Sql.ConnectionError
  | MigrationNothingToRollback
  | MigrationGotDivergents
  | MigrationException !E.SomeException
  | MigrationNotFound !Text
  | MigrationBeforeUpError !Text
  | MigrationBeforeDownError !Text
  deriving (Show)

prettyMigrationError :: MigrationError -> Doc
prettyMigrationError = \case
  MigrationCheckError qe -> "Check error" <+> prettyQueryError qe
  MigrationUpError pending qe -> "Up error" <+> prettyPending pending <+> prettyQueryError qe
  MigrationDownError up qe -> "Down error" <+> prettyUp up <+> prettyQueryError qe
  MigrationForceUpError (SomeMigration m) qe -> "Forced up error" <+> R.pretty (migrationName m) <+> prettyQueryError qe
  MigrationForceDownError (SomeMigration m) qe -> "Forced down error" <+> R.pretty (migrationName m) <+> prettyQueryError qe
  MigrationDivergentDownError up qe -> "Divergent down error" <+> prettyDivergent up <+> prettyQueryError qe
  MigrationConnectError connerr -> "Connection error" <+> R.viaShow connerr
  MigrationNothingToRollback -> "Nothing to roll back"
  MigrationGotDivergents -> "Divergent migrations"
  MigrationException se -> R.viaShow se
  MigrationNotFound name -> "Migration not found:" <+> R.pretty name
  MigrationBeforeDownError err -> "Before down error:" <+> R.pretty err
  MigrationBeforeUpError err -> "Before up error:" <+> R.pretty err
  where
    prettyQueryError = \case
      Sql.QueryError bs params cmderr ->
        R.vsep
          [ "QueryError for  " <+> R.align (R.vsep (map R.pretty (Text.lines (Text.decodeUtf8 bs))))
          , " - Params: " <+> R.align (R.list (map R.pretty params))
          , " - Command Error: " <+> R.align (prettyClientError cmderr)
          ]
      Sql.PipelineError cmderr ->
        R.vsep
          [ "PipelineError"
          , prettyClientError cmderr
          ]

    prettyClientError = \case
      Sql.ClientError mc -> "ClientError " <+> foldMap (R.pretty . Text.decodeUtf8) mc
      Sql.ResultError re -> "ResultError " <+> prettyResultError re

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
  => PerformMigrationOpts
  -> MigrationCli
  -> IO (Either MigrationError ())
performMigrations PerformMigrationOpts {beforeUp, beforeDown, afterUp, afterDown} MigrationCli {db = MigrationDB {acquire, release, run}, cmd} = runResourceT $ runExceptT do
  (_releaseKey, mdb) <- allocate acquire \case
    Left _ -> pure ()
    Right db -> release db
  db <- errBy MigrationConnectError (pure mdb)
  let
    check = errBy MigrationCheckError (runSession (checkMigrations @migrations))

    runSession :: (MonadIO m) => Sql.Session a -> m (Either Sql.SessionError a)
    runSession s = liftIO (run s db)

    runInCorrectDirectory :: (Migration m) => m -> IO a -> IO a
    runInCorrectDirectory =
      maybe
        id
        ( \dir io -> do
            exists <- doesDirectoryExist dir
            if exists
              then withCurrentDirectory dir io
              else io
        )
        . directory

    runPending :: (Migration m, MonadIO io) => m -> ExceptT MigrationError io UTCTime
    runPending m = mapExceptT catchLiftIO do
      liftIO . putDoc . prettyPending $ PendingMigration m
      withExceptT MigrationBeforeUpError (ExceptT (beforeUp m))
      t <- withExceptT (MigrationUpError (PendingMigration m)) . ExceptT . runInCorrectDirectory m . runSession $ Tx.transaction Tx.Serializable Tx.Write do
        Tx.sql (Text.encodeUtf8 (up m))
        Tx.statement
          (migrationName m, up m, down m)
          [Sql.singletonStatement|
            INSERT INTO hasql_mover_migration (name, up, down) VALUES($1::text, $2::text, $3::text)
            RETURNING executed_at::timestamptz
          |]
      liftIO (afterUp m)
      pure t

    runRollback :: (Migration m, MonadIO io) => m -> UTCTime -> Text -> ExceptT MigrationError io ()
    runRollback m u downSql = mapExceptT catchLiftIO do
      liftIO . putDoc . prettyRollback $ Rollback m
      withExceptT MigrationBeforeDownError (ExceptT (beforeDown m))
      t <- withExceptT (MigrationDownError (UpMigration m u)) . ExceptT . runInCorrectDirectory m . runSession $ Tx.transaction Tx.Serializable Tx.Write do
        Tx.sql (Text.encodeUtf8 downSql)
        case cast m of
          Just BaseMigration -> pure ()
          Nothing -> Tx.statement (migrationName m) [Sql.resultlessStatement|DELETE FROM hasql_mover_migration WHERE name = ($1::text)|]
      liftIO (afterDown m)
      pure t

    catchLiftIO :: (MonadIO m) => IO (Either MigrationError a) -> m (Either MigrationError a)
    catchLiftIO m = liftIO do
      E.try m <&> \case
        Left err -> Left (MigrationException err)
        Right a -> a

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
            (migrationName m == name)
            (put (Just (SomeMigration m)))

        allMigrations = cpure_NP (Proxy @Migration) (UnknownMigration migration) :: NP UnknownMigration migrations

  case cmd of
    -- Status
    MigrateStatus -> liftIO (putDoc (ppStatus "Current migrations status:" checked))
    -- Up
    MigrateUp
      | null divergents -> do
          forM_ pendings \PendingMigration {migration} -> do
            _ <- runPending migration
            liftIO . putDoc . ppStatus "New migrations status:" =<< check
      | otherwise -> throwE MigrationGotDivergents
    -- Down
    MigrateDown {undoDivergents, divergentUseOldDown}
      | null ups && (null divergents && not undoDivergents) -> throwE MigrationNothingToRollback
      | undoDivergents && not (null divergents)
      , DivergentMigration {migration, executedAt, oldDown} <- last divergents ->
          runRollback migration executedAt (if divergentUseOldDown then oldDown else down migration)
      | not (null divergents) ->
          throwE MigrationGotDivergents
      | UpMigration {migration, executedAt} <- last ups -> do
          runRollback migration executedAt (down migration)
          liftIO . putDoc . ppStatus "New migrations status:" =<< check
    -- Forcing down
    MigrateForceDown nameText | Just (SomeMigration m) <- findMigrationByName nameText -> do
      now <- liftIO getCurrentTime
      runRollback m now (down m)
    MigrateForceDown nameText -> throwE (MigrationNotFound nameText)
    -- Forcing up
    MigrateForceUp nameText | Just (SomeMigration m) <- findMigrationByName nameText -> void do
      runPending m
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
        , R.hardline
        ]

    title f c = R.annotate (f c)
    ppExecAt e = fromString (Time.formatTime Time.defaultTimeLocale "%FT%R" e)

    ppUp UpMigration {migration, executedAt} =
      R.hsep [title color Green ("[=] Up" <+> ppExecAt executedAt), R.align (R.pretty (migrationName migration))]
    ppDivergent DivergentMigration {migration, executedAt} =
      R.hsep [title color Red ("[d] Up" <+> ppExecAt executedAt), R.align (R.pretty (migrationName migration))]
    ppPending PendingMigration {migration} =
      R.hsep [title colorDull Blue "[ ] Pending            ", R.align (R.pretty (migrationName migration))]

    errBy f a = withExceptT f (ExceptT a)

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

-- | Declare a migration from a directory.
--
-- Expected directory structure:
-- "./migrations/NAME/up.sql" - SQL to perform on migration
-- "./migrations/NAME/down.sql" - SQL to perform on rollback
declareMigrationFromDirectory :: Text -> TH.DecsQ
declareMigrationFromDirectory name = do
  qtype <- TH.newName (Text.unpack name)
  qconstr <- TH.newName (Text.unpack name)
  dec <- TH.dataD (pure []) qtype [] Nothing [TH.normalC qconstr []] [TH.derivClause (Just TH.StockStrategy) [[t|Show|]]]
  upSql <- Text.strip <$> (TH.addDependentFile upFile >> TH.runIO (Text.readFile upFile))
  downSql <- Text.strip <$> (TH.addDependentFile downFile >> TH.runIO (Text.readFile downFile))
  inst <-
    [d|
      instance Migration $(TH.conT qtype) where
        migration = $(TH.conE qconstr)
        up _ = $(TH.lift (Text.unpack upSql))
        down _ = $(TH.lift (Text.unpack downSql))
      |]
  pure (dec : inst)
  where
    migrationDir = "migrations/" <> name
    upFile = Text.unpack (migrationDir <> "/up.sql")
    downFile = Text.unpack (migrationDir <> "/down.sql")

-- | Declare a migration (via 'SMigration') from a directory.
--
-- Expected directory structure:
-- "./migrations/NAME/up.sql" - SQL to perform on migration
-- "./migrations/NAME/down.sql" - SQL to perform on rollback
migrationFromDirectory :: Text -> TH.TypeQ
migrationFromDirectory name = do
  let text = TH.litT . TH.strTyLit . Text.unpack
  upSql <- Text.strip <$> (TH.addDependentFile upFile >> TH.runIO (Text.readFile upFile))
  downSql <- Text.strip <$> (TH.addDependentFile downFile >> TH.runIO (Text.readFile downFile))
  [t|SMigration $(text name) ('Just $(text migrationDir)) $(text upSql) $(text downSql)|]
  where
    migrationDir = "migrations/" <> name
    upFile = Text.unpack (migrationDir <> "/up.sql")
    downFile = Text.unpack (migrationDir <> "/down.sql")

--
-- TH.QuasiQuoter
--   { quoteExp = undefined
--   , quoteType = undefined
--   , quotePat = undefined
--   , quoteDec = \s ->
--       case M.parse parseMigrationDesc "hasql-mover" (Text.pack s) of
--         Left err -> error (M.errorBundlePretty err)
--         Right MigrationDesc {name, up, down} -> do
--           qtype <- TH.newName (Text.unpack name)
--           qconstr <- TH.newName (Text.unpack name)
--           dec <- TH.dataD (pure []) qtype [] Nothing [TH.normalC qconstr []] [TH.derivClause (Just TH.StockStrategy) [[t|Show|]]]
--           inst <-
--             [d|
--               instance Migration $(TH.conT qtype) where
--                 migration = $(TH.conE qconstr)
--                 up _ = $(TH.lift (Text.unpack up))
--                 down _ = $(TH.lift (Text.unpack down))
--               |]
--           pure (dec : inst)
--   }

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
