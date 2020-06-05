module Monadoc.Type.MigrationSpec
  ( spec
  )
where

import qualified Data.Text as Text
import qualified Monadoc.Type.Migration as Monadoc
import qualified Monadoc.Type.Sha256 as Monadoc
import qualified Monadoc.Type.Timestamp as Monadoc
import qualified Monadoc.Vendor.Sqlite as Sql
import qualified Monadoc.Vendor.Time as Time
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "sha256" $ do

    Hspec.it "returns the digest of the query" $ do
      let
        migration = Monadoc.Migration
          { Monadoc.query = Sql.Query $ Text.pack ""
          , Monadoc.timestamp = Monadoc.fromUtcTime $ Time.posixSecondsToUTCTime 0
          }
      Monadoc.sha256 migration `Hspec.shouldBe` Monadoc.fromDigest (read "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

  Hspec.describe "toRow" $ do

    Hspec.it "converts into a SQL row" $ do
      let
        migration = Monadoc.Migration
          { Monadoc.query = Sql.Query $ Text.pack ""
          , Monadoc.timestamp = Monadoc.fromUtcTime $ Time.posixSecondsToUTCTime 0
          }
      Sql.toRow migration `Hspec.shouldBe`
        [ Sql.SQLText $ Text.pack "1970-01-01T00:00:00.000Z"
        , Sql.SQLText $ Text.pack "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        ]
