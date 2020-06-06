module Monadoc.Type.UrlSpec
  ( spec
  )
where

import qualified Data.Text as Text
import qualified Monadoc.Type.Url as Url
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.URI as Uri
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "fromField" $ do

    Hspec.it "parses a basic URL" $ do
      let
        field = Sql.Field (Sql.SQLText (Text.pack "http://monadoc.test")) 0
        url = Url.fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
      Sql.fromField field `Hspec.shouldBe` pure url

    Hspec.it "fails to parse an invalid URL" $ do
      let field = Sql.Field (Sql.SQLText (Text.pack "not valid")) 0
      Sql.fromField field `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Url.Url)

  Hspec.describe "toField" $ do

    Hspec.it "renders a basic URL" $ do
      let
        url = Url.fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
        sqlData = Sql.SQLText $ Text.pack "http://monadoc.test"
      Sql.toField url `Hspec.shouldBe` sqlData
