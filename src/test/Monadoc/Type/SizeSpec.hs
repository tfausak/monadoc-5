module Monadoc.Type.SizeSpec where

import qualified Monadoc.Type.Size as Size
import qualified Monadoc.Vendor.Sql as Sql
import Test

spec :: Spec
spec = describe "Monadoc.Type.Size" $ do

  describe "fromField" $ do

    it "converts from an integer" $ do
      let
        field = Sql.Field (Sql.SQLInteger 123) 0
        size = Size.fromInt 123
      Sql.fromField field `shouldBe` pure size

  describe "toField" $ do

    it "converts to an integer" $ do
      let
        size = Size.fromInt 123
        sqlData = Sql.SQLInteger 123
      Sql.toField size `shouldBe` sqlData
