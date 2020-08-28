module Monadoc.Type.MigrationSpec where

import qualified Data.Time.Clock.POSIX as Time
import qualified Database.SQLite.Simple as Sql
import Monadoc.Prelude
import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Migration" <| do

  describe "sha256" <| do

    it "returns the digest of the query" <| do
      let
        migration = Migration.Migration
          { Migration.query = ""
          , Migration.timestamp = Timestamp.fromUtcTime
            <| Time.posixSecondsToUTCTime 0
          }
      expected <-
        read "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          |> maybe (fail "invalid digest") (Sha256.fromDigest >>> pure)
      Migration.sha256 migration `shouldBe` expected

  describe "toRow" <| do

    it "converts into a SQL row" <| do
      let
        migration = Migration.Migration
          { Migration.query = ""
          , Migration.timestamp = Timestamp.fromUtcTime
            <| Time.posixSecondsToUTCTime 0
          }
        row =
          [ Sql.SQLText "1970-01-01T00:00:00.000Z"
          , Sql.SQLText
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          ]
      Sql.toRow migration `shouldBe` row
