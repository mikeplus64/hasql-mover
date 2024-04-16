{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hasql.Mover.Type where

import Crypto.Hash (Digest, SHA1, hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Typeable (eqT, (:~:) (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

data MigrationName sym where
  MigrationName :: (KnownSymbol sym) => {getMigrationName :: Text} -> MigrationName sym

eqM :: MigrationName a -> MigrationName b -> Maybe (a :~: b)
eqM (MigrationName _) (MigrationName _) = eqT

--------------------------------------------------------------------------------

instance (KnownSymbol sym, sym ~ sym') => IsLabel sym (MigrationName sym') where
  fromLabel = MigrationName (toText (symbolVal (Proxy @sym)))

data MigrationStatus = Up | Divergent | Pending
  deriving stock (Show, Read, Eq, Ord)

data MigrationDescription name = MigrationDescription
  { name :: MigrationName name
  , up, down :: Text
  }

data CheckedMigration name
  = PendingMigration
      { name :: MigrationName name
      , up, down :: (ByteString, Digest SHA1)
      }
  | DivergentMigration
      { name :: MigrationName name
      , newUp, newDown, oldUp, oldDown :: (ByteString, Digest SHA1)
      }
