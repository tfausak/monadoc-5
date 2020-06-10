module Monadoc.Type.EtagSpec
  ( spec
  )
where

import qualified Data.Text as Text
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Etag" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "parses an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        field = Sql.Field (Sql.SQLText . Text.pack $ show byteString) 0
        etag = Etag.fromByteString byteString
      Sql.fromField field `Hspec.shouldBe` pure etag

    Hspec.it "fails to parse an invalid ETag" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Etag.Etag)

  Hspec.describe "toField" $ do

    Hspec.it "renders an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        etag = Etag.fromByteString byteString
        sqlData = Sql.SQLText . Text.pack $ show byteString
      Sql.toField etag `Hspec.shouldBe` sqlData
