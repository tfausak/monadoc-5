module Monadoc.Type.EtagSpec
  ( spec
  )
where

import qualified Data.Text as Text
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Etag" $ do

  Test.describe "fromField" $ do

    Test.it "parses an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        field = Sql.Field (Sql.SQLText . Text.pack $ show byteString) 0
        etag = Etag.fromByteString byteString
      Sql.fromField field `Test.shouldBe` pure etag

    Test.it "fails to parse an invalid ETag" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `Test.shouldBe` (Sql.Errors [] :: Sql.Ok Etag.Etag)

  Test.describe "toField" $ do

    Test.it "renders an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        etag = Etag.fromByteString byteString
        sqlData = Sql.SQLText . Text.pack $ show byteString
      Sql.toField etag `Test.shouldBe` sqlData
