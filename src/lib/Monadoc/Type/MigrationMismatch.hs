module Monadoc.Type.MigrationMismatch where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Time as Time
import qualified Test.Hspec as Hspec

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

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.MigrationMismatch" $ do

  Hspec.describe "displayException" $ do

    Hspec.it "looks nice" $ do
      let
        e = replicate 64 '0'
        a = replicate 64 '1'
        migrationMismatch = MigrationMismatch
          { actual = Sha256.fromDigest $ read a
          , expected = Sha256.fromDigest $ read e
          , timestamp = Timestamp.fromUtcTime $ Time.posixSecondsToUTCTime 0
          }
        string = mconcat
          ["migration 1970-01-01 00:00:00 UTC expected ", e, " but got ", a]
      Exception.displayException migrationMismatch `Hspec.shouldBe` string
