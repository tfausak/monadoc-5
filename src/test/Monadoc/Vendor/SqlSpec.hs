module Monadoc.Vendor.SqlSpec where

import qualified Monadoc.Vendor.Sql as Sql
import Test
import qualified Text.Read as Read

spec :: Spec
spec = describe "Monadoc.Vendor.Sql" $ do

  describe "fromFieldVia" $ do

    it "handles success" $ do
      let field = Sql.Field (Sql.SQLText "()") 0
      Sql.fromFieldVia Read.readMaybe field `shouldBe` Sql.Ok ()

    it "handles failure" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromFieldVia Read.readMaybe field
        `shouldBe` (Sql.Errors [] :: Sql.Ok ())
