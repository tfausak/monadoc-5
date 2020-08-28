module Monadoc.Type.MigrationMismatchSpec where

import qualified Control.Monad.Catch as Exception
import qualified Data.Time.Clock.POSIX as Time
import Monadoc.Prelude
import qualified Monadoc.Type.MigrationMismatch as MigrationMismatch
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.MigrationMismatch" <| do

  describe "displayException" <| do

    it "looks nice" <| do
      expected <- replicate 64 '0' |> read |> maybe
        (fail "invalid digest")
        (Sha256.fromDigest >>> pure)
      actual <- replicate 64 '1' |> read |> maybe
        (fail "invalid digest")
        (Sha256.fromDigest >>> pure)
      let
        migrationMismatch = MigrationMismatch.MigrationMismatch
          { MigrationMismatch.actual = actual
          , MigrationMismatch.expected = expected
          , MigrationMismatch.timestamp = Timestamp.fromUtcTime
            <| Time.posixSecondsToUTCTime 0
          }
        string = fold
          [ "migration 1970-01-01 00:00:00 UTC expected "
          , replicate 64 '0'
          , " but got "
          , replicate 64 '1'
          ]
      Exception.displayException migrationMismatch `shouldBe` string
