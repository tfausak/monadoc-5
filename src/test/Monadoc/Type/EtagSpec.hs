module Monadoc.Type.EtagSpec where

import qualified Data.Text as Text
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Vendor.Sql as Sql
import Test

spec :: Spec
spec = describe "Monadoc.Type.Etag" $ do

  describe "fromField" $ do

    it "parses an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        field = Sql.Field (Sql.SQLText . Text.pack $ show byteString) 0
        etag = Etag.fromByteString byteString
      Sql.fromField field `shouldBe` pure etag

    it "fails to parse an invalid ETag" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `shouldBe` (Sql.Errors [] :: Sql.Ok Etag.Etag)

  describe "toField" $ do

    it "renders an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        etag = Etag.fromByteString byteString
        sqlData = Sql.SQLText . Text.pack $ show byteString
      Sql.toField etag `shouldBe` sqlData
