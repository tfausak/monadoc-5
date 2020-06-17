module Monadoc.Type.UrlSpec
  ( spec
  )
where

import qualified Monadoc.Type.Url as Url
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.URI as Uri
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Url" $ do

  Test.describe "fromField" $ do

    Test.it "parses a basic URL" $ do
      let
        field = Sql.Field (Sql.SQLText "http://monadoc.test") 0
        url = Url.fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
      Sql.fromField field `Test.shouldBe` pure url

    Test.it "fails to parse an invalid URL" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `Test.shouldBe` (Sql.Errors [] :: Sql.Ok Url.Url)

  Test.describe "toField" $ do

    Test.it "renders a basic URL" $ do
      let
        url = Url.fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
        sqlData = Sql.SQLText "http://monadoc.test"
      Sql.toField url `Test.shouldBe` sqlData
