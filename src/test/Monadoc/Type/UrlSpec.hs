module Monadoc.Type.UrlSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude
import qualified Monadoc.Type.Url as Url
import qualified Network.URI as Uri
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Url" $ do

  describe "fromField" $ do

    it "parses a basic URL" $ do
      let
        field = Sql.Field (Sql.SQLText "http://monadoc.test") 0
        url = Url.fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
      Sql.fromField field `shouldBe` pure url

    it "fails to parse an invalid URL" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `shouldBe` (Sql.Errors [] :: Sql.Ok Url.Url)

  describe "toField" $ do

    it "renders a basic URL" $ do
      let
        url = Url.fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
        sqlData = Sql.SQLText "http://monadoc.test"
      Sql.toField url `shouldBe` sqlData
