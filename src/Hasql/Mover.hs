module Hasql.Mover (
  Migration (..),

  -- * Declaration
  declareMigration,

  -- * Rollbacks
  Rollback (..),

  -- * Checking and running migrations
  CheckedMigrations,
  checkMigrations,
  runPendingMigrations,
) where

import Control.Monad (forM_, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), execStateT, gets, modify')
import Data.Char (isSpace)
import Data.Proxy (Proxy (..))
import Data.SOP.Constraint (All)
import Data.SOP.NP (NP (..), cpure_NP, traverse__NP)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Typeable (Typeable, cast)
import Data.Void (Void)
import Hasql.Session qualified as Sql
import Hasql.TH qualified as Sql
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Tx
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import PyF (strTrim)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
-- Checked migrations -- these have been checked against the current database
-- status

data PendingMigration = forall m. (Migration m) => PendingMigration {migration :: m}
data UpMigration = forall m. (Migration m) => UpMigration {migration :: m, executedAt :: UTCTime}
data DivergentMigration = forall m. (Migration m) => DivergentMigration {migration :: m, oldUp, oldDown :: Text, executedAt :: UTCTime}
data UnknownMigration m = (Migration m) => UnknownMigration

data CheckedMigrations names = CheckedMigrations
  { ups :: [UpMigration]
  , divergents :: [DivergentMigration]
  , pendings :: [PendingMigration]
  }

class (Typeable a, Show a) => Migration a where
  migration :: a
  up :: Text
  down :: Text

migrationName :: forall a. (Migration a) => Text
migrationName = Text.pack (show (migration @a))

--------------------------------------------------------------------------------
-- The base migration

data BaseMigration = BaseMigration
  deriving stock (Show, Read, Eq, Ord)

instance Migration BaseMigration where
  migration = BaseMigration
  up =
    [strTrim|
    CREATE TABLE hasql_mover_migration (
      id serial NOT NULL,
      name text NOT NULL,
      up text NOT NULL,
      down text NOT NULL,
      executed_at timestamptz NOT NULL DEFAULT now()
    )|]
  down = [strTrim|DROP TABLE hasql_mover_migration CASCADE|]

newtype Rollback m = Rollback m
  deriving stock (Show)

instance (Migration m) => Migration (Rollback m) where
  migration = Rollback migration
  up = down @m
  down = up @m

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
        then checkMigration @BaseMigration
        else do
          modify' \s -> s {haveBaseTable = False}
          addPending @BaseMigration

    checkOthers :: CheckM ()
    checkOthers =
      traverse__NP
        (\(UnknownMigration @m) -> checkMigration @m)
        (cpure_NP (Proxy @Migration) UnknownMigration :: NP UnknownMigration migrations)

    checkMigration :: forall m. (Migration m) => CheckM ()
    checkMigration = do
      canContinue <- gets \CheckState {haveBaseTable, divergents} -> haveBaseTable && null divergents
      if canContinue
        then do
          r <-
            lift $
              Tx.statement
                (migrationName @m)
                [Sql.maybeStatement|
                  SELECT up::text, down::text, executed_at::timestamptz
                  FROM hasql_mover_migration
                  WHERE name = $1::text
                |]
          case r of
            Just (oldUp, oldDown, executedAt)
              | oldUp == up @m -> addUp @m executedAt
              | otherwise -> addDivergent @m executedAt oldUp oldDown
            Nothing -> addPending @m
        else addPending @m

    addDivergent :: forall m. (Migration m) => UTCTime -> Text -> Text -> CheckM ()
    addDivergent executedAt oldUp oldDown =
      modify' \CheckState {..} -> CheckState {divergents = DivergentMigration {migration = migration @m, ..} : divergents, ..}

    addPending :: forall m. (Migration m) => CheckM ()
    addPending =
      modify' \CheckState {..} -> CheckState {pendings = PendingMigration {migration = migration @m, ..} : pendings, ..}

    addUp :: forall m. (Migration m) => UTCTime -> CheckM ()
    addUp executedAt =
      modify' \CheckState {..} -> CheckState {ups = UpMigration {migration = migration @m, ..} : ups, ..}

runPendingMigrations :: CheckedMigrations names -> Sql.Session ()
runPendingMigrations CheckedMigrations {pendings} = do
  forM_ pendings \(PendingMigration (m :: m)) -> do
    case cast m of
      Just BaseMigration -> Tx.transaction Tx.Serializable Tx.Write $ Tx.sql $ Text.encodeUtf8 $ up @BaseMigration
      _ -> pure ()
    _now <-
      Tx.transaction Tx.Serializable Tx.Write $
        Tx.statement
          (migrationName @m, up @m, down @m)
          [Sql.singletonStatement|
        INSERT INTO hasql_mover_migration (name, up, down) VALUES($1::text, $2::text, $3::text)
        RETURNING time::timestamptz
      |]
    pure ()

-- (`evalStateT` True) . traverse'_NP \m -> case m of
--   UpMigration _ -> pure m
--   DivergentMigration {oldUp, oldDown, executedAt} -> do
--     put False
--     pure m
--   PendingMigration @m -> do
--     canRun <- get
--     if canRun
--       then do
--         pure (UpMigration @m now)
--       else do
--         pure (PendingMigration @m)

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
            dec <- TH.dataD (pure []) qtype [] Nothing [TH.normalC qconstr []] [TH.derivClause Nothing [[t|Show|]]]
            inst <-
              [d|
                instance Migration $(TH.conT qtype) where
                  migration = $(TH.conE qconstr)
                  up = $(TH.lift (Text.unpack up))
                  down = $(TH.lift (Text.unpack down))
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

line :: P Text
line =
  M.label "a line of text" $
    M.takeWhile1P Nothing (/= '\n') <* M.try M.newline
