module Monadoc.Type.Binary where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

-- | Some binary data. Just a 'ByteString.ByteString' under the hood. Use
-- 'fromByteString' and 'toByteString' to work with these values.
newtype Binary
  = Binary ByteString.ByteString
  deriving (Eq, Show)

instance Sql.FromField Binary where
  fromField = fmap fromByteString . Sql.fromField

instance Sql.ToField Binary where
  toField = Sql.toField . toByteString

fromByteString :: ByteString.ByteString -> Binary
fromByteString = Binary

toByteString :: Binary -> ByteString.ByteString
toByteString (Binary byteString) = byteString

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Binary" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "converts from a blob" $ do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        field = Sql.Field (Sql.SQLBlob byteString) 0
        binary = fromByteString byteString
      Sql.fromField field `Hspec.shouldBe` pure binary

  Hspec.describe "toField" $ do

    Hspec.it "converts to a blob" $ do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        binary = fromByteString byteString
        sqlData = Sql.SQLBlob byteString
      Sql.toField binary `Hspec.shouldBe` sqlData
