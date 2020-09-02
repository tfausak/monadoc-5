module Monadoc.Type.BinarySpec where

import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude
import qualified Monadoc.Type.Binary as Monadoc
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Binary" <| do

  describe "fromField" <| do

    it "converts from a blob" <| do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        field = Sql.Field (Sql.SQLBlob byteString) 0
        binary = Monadoc.fromByteString byteString
      Sql.fromField field `shouldBe` pure binary

  describe "toField" <| do

    it "converts to a blob" <| do
      let
        byteString = ByteString.pack [0x00, 0x01, 0x0f, 0x10, 0xf0, 0xff]
        binary = Monadoc.fromByteString byteString
        sqlData = Sql.SQLBlob byteString
      Sql.toField binary `shouldBe` sqlData
