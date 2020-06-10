module Monadoc.Type.MigrationSpec
  ( spec
  )
where

import qualified Monadoc.Type.Migration as Migration
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Vendor.Time as Time
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Migration" $ do

  Hspec.describe "sha256" $ do

    Hspec.it "returns the digest of the query" $ do
      let
        migration = Migration.Migration
          { Migration.query = ""
          , Migration.timestamp = Timestamp.fromUtcTime
            $ Time.posixSecondsToUTCTime 0
          }
      Migration.sha256 migration `Hspec.shouldBe` Sha256.fromDigest
        (read
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        )

  Hspec.describe "toRow" $ do

    Hspec.it "converts into a SQL row" $ do
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
      Sql.toRow migration `Hspec.shouldBe` row
