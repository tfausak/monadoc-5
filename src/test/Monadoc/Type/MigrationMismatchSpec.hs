module Monadoc.Type.MigrationMismatchSpec where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Time as Time
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.MigrationMismatch" $ do

  describe "displayException" $ do

    it "looks nice" $ do
      let
        expected = replicate 64 '0'
        actual = replicate 64 '1'
        migrationMismatch = MigrationMismatch.MigrationMismatch
          { MigrationMismatch.actual = Sha256.fromDigest $ read actual
          , MigrationMismatch.expected = Sha256.fromDigest $ read expected
          , MigrationMismatch.timestamp = Timestamp.fromUtcTime
            $ Time.posixSecondsToUTCTime 0
          }
        string = mconcat
          [ "migration 1970-01-01 00:00:00 UTC expected "
          , expected
          , " but got "
          , actual
          ]
      Exception.displayException migrationMismatch `shouldBe` string
