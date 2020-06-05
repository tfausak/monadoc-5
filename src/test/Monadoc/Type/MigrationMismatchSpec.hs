module Monadoc.Type.MigrationMismatchSpec
  ( spec
  )
where

import qualified Control.Exception as Exception
import qualified Monadoc.Type.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Time as Time
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.it "displayException" $ do
    let
      migrationMismatch = MigrationMismatch.MigrationMismatch
        { MigrationMismatch.actual = Sha256.fromDigest . read $ replicate 64 '1'
        , MigrationMismatch.expected = Sha256.fromDigest . read $ replicate 64 '0'
        , MigrationMismatch.timestamp = Timestamp.fromUtcTime $ Time.posixSecondsToUTCTime 0
        }
    Exception.displayException migrationMismatch `Hspec.shouldBe` "migration 1970-01-01 00:00:00 UTC expected 0000000000000000000000000000000000000000000000000000000000000000 but got 1111111111111111111111111111111111111111111111111111111111111111"
