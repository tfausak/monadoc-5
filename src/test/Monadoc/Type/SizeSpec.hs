module Monadoc.Type.SizeSpec where

import qualified Monadoc.Type.Size as Size
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Size" $ do

  Test.describe "fromField" $ do

    Test.it "converts from an integer" $ do
      let
        field = Sql.Field (Sql.SQLInteger 123) 0
        size = Size.fromInt 123
      Sql.fromField field `Test.shouldBe` pure size

  Test.describe "toField" $ do

    Test.it "converts to an integer" $ do
      let
        size = Size.fromInt 123
        sqlData = Sql.SQLInteger 123
      Sql.toField size `Test.shouldBe` sqlData
