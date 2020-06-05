module Monadoc.Vendor.SqlSpec
  ( spec
  )
where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "sql" $ do

    Hspec.it "converts into a query" $ do
      Sql.sql "" `Hspec.shouldBe` Sqlite.Query Text.empty
