module Monadoc.Type.Url where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.URI as Uri
import qualified Test.Hspec as Hspec

-- | A uniform resource locator. Behind the scenes this is a 'Uri.URI'. Use
-- 'fromUri' and 'toUri' to convert into and out of this type. Note that
-- internationalized resource identifiers (IRIs) are not supported.
newtype Url
  = Url Uri.URI
  deriving (Eq, Show)

instance Sql.FromField Url where
  fromField = Sql.fromFieldVia $ fmap fromUri . Uri.parseURI

instance Sql.ToField Url where
  toField = Sql.toField . ($ "") . Uri.uriToString id . toUri

fromUri :: Uri.URI -> Url
fromUri = Url

toUri :: Url -> Uri.URI
toUri (Url uri) = uri

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Url" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "parses a basic URL" $ do
      let
        field = Sql.Field (Sql.SQLText "http://monadoc.test") 0
        url = fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
      Sql.fromField field `Hspec.shouldBe` pure url

    Hspec.it "fails to parse an invalid URL" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Url)

  Hspec.describe "toField" $ do

    Hspec.it "renders a basic URL" $ do
      let
        url = fromUri $ Uri.URI
          "http:"
          (Just (Uri.URIAuth "" "monadoc.test" ""))
          ""
          ""
          ""
        sqlData = Sql.SQLText "http://monadoc.test"
      Sql.toField url `Hspec.shouldBe` sqlData
