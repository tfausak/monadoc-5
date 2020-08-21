module Monadoc.Type.Etag where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec
import qualified Text.Read as Read

-- | An HTTP entity tag (ETag). Values typically come from HTTP headers, so
-- this is a 'ByteString.ByteString' behind the scenes. Usually values are
-- quoted ASCII strings like @"01ef"@. Use 'fromByteString' and 'toByteString'
-- for conversion.
newtype Etag
  = Etag ByteString.ByteString
  deriving (Eq, Show)

instance Sql.FromField Etag where
  fromField = Sql.fromFieldVia $ fmap fromByteString . Read.readMaybe

instance Sql.ToField Etag where
  toField = Sql.toField . show . toByteString

fromByteString :: ByteString.ByteString -> Etag
fromByteString = Etag

toByteString :: Etag -> ByteString.ByteString
toByteString (Etag byteString) = byteString

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Etag" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "parses an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        field = Sql.Field (Sql.SQLText . Text.pack $ show byteString) 0
        etag = fromByteString byteString
      Sql.fromField field `Hspec.shouldBe` pure etag

    Hspec.it "fails to parse an invalid ETag" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Etag)

  Hspec.describe "toField" $ do

    Hspec.it "renders an ETag" $ do
      let
        byteString = "\"0123456789aBcDeF\""
        etag = fromByteString byteString
        sqlData = Sql.SQLText . Text.pack $ show byteString
      Sql.toField etag `Hspec.shouldBe` sqlData
