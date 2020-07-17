module Monadoc.Type.BinarySpec where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Type.Binary as Monadoc
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Binary" $ do

  Test.describe "fromField" $ do

    Test.it "converts from a blob" $ do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        field = Sql.Field (Sql.SQLBlob byteString) 0
        binary = Monadoc.fromByteString byteString
      Sql.fromField field `Test.shouldBe` pure binary

  Test.describe "toField" $ do

    Test.it "converts to a blob" $ do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        binary = Monadoc.fromByteString byteString
        sqlData = Sql.SQLBlob byteString
      Sql.toField binary `Test.shouldBe` sqlData
