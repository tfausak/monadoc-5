module Monadoc.Type.MigrationSpec where

import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Vendor.Time as Time
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Migration" $ do

  describe "sha256" $ do

    it "returns the digest of the query" $ do
      let
        migration = Migration.Migration
          { Migration.query = ""
          , Migration.timestamp = Timestamp.fromUtcTime
            $ Time.posixSecondsToUTCTime 0
          }
      Migration.sha256 migration `shouldBe` Sha256.fromDigest
        (read
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        )

  describe "toRow" $ do

    it "converts into a SQL row" $ do
      let
        migration = Migration.Migration
          { Migration.query = ""
          , Migration.timestamp = Timestamp.fromUtcTime
            $ Time.posixSecondsToUTCTime 0
          }
        row =
          [ Sql.SQLText "1970-01-01T00:00:00.000Z"
          , Sql.SQLText
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          ]
      Sql.toRow migration `shouldBe` row
