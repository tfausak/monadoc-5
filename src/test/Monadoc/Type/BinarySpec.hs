module Monadoc.Type.BinarySpec
  ( spec
  )
where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Type.Binary as Monadoc
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Binary" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "converts from a blob" $ do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        field = Sql.Field (Sql.SQLBlob byteString) 0
        binary = Monadoc.fromByteString byteString
      Sql.fromField field `Hspec.shouldBe` pure binary

  Hspec.describe "toField" $ do

    Hspec.it "converts to a blob" $ do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        binary = Monadoc.fromByteString byteString
        sqlData = Sql.SQLBlob byteString
      Sql.toField binary `Hspec.shouldBe` sqlData
