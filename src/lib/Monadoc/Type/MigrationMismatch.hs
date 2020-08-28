module Monadoc.Type.MigrationMismatch where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import Monadoc.Prelude

-- | A content mismatch when running a migration. This is thrown when the
-- digest of a migration has changed since it was ran. Identifying these cases
-- is useful to ensure data consistency.
data MigrationMismatch = MigrationMismatch
  { actual :: Sha256.Sha256
  , expected :: Sha256.Sha256
  , timestamp :: Timestamp.Timestamp
  } deriving (Eq, Show)

instance Exception.Exception MigrationMismatch where
  displayException migrationMismatch = unwords
    [ "migration"
    , show . Timestamp.toUtcTime $ timestamp migrationMismatch
    , "expected"
    , show . Sha256.toDigest $ expected migrationMismatch
    , "but got"
    , show . Sha256.toDigest $ actual migrationMismatch
    ]
