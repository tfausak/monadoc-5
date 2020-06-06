module Monadoc.Vendor.SqlSpec
  ( spec
  )
where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec
import qualified Text.Read as Read

spec :: Hspec.Spec
spec = do

  Hspec.describe "fromFieldVia" $ do

    Hspec.it "handles success" $ do
      let field = Sql.Field (Sql.SQLText $ Text.pack "()") 0
      Sql.fromFieldVia Read.readMaybe field `Hspec.shouldBe` Sql.Ok ()

    Hspec.it "handles failure" $ do
      let field = Sql.Field (Sql.SQLText $ Text.pack "not valid") 0
      Sql.fromFieldVia Read.readMaybe field
        `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok ())

  Hspec.describe "sql" $ do

    Hspec.it "converts into a query" $ do
      Sql.sql "" `Hspec.shouldBe` Sqlite.Query Text.empty
