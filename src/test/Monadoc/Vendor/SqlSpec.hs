module Monadoc.Vendor.SqlSpec where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Test
import qualified Text.Read as Read

spec :: Test.Spec
spec = Test.describe "Monadoc.Vendor.Sql" $ do

  Test.describe "fromFieldVia" $ do

    Test.it "handles success" $ do
      let field = Sql.Field (Sql.SQLText "()") 0
      Sql.fromFieldVia Read.readMaybe field `Test.shouldBe` Sql.Ok ()

    Test.it "handles failure" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromFieldVia Read.readMaybe field
        `Test.shouldBe` (Sql.Errors [] :: Sql.Ok ())
