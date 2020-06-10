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
spec = Hspec.describe "Monadoc.Type.MigrationMismatch" $ do

  Hspec.describe "displayException" $ do

    Hspec.it "looks nice" $ do
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
      Exception.displayException migrationMismatch `Hspec.shouldBe` string
