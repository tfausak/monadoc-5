module Monadoc.Type.Sha256Spec where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude
import qualified Monadoc.Type.Sha256 as Sha256
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Sha256" <| do

  describe "fromField" <| do

    it "parses a SHA-256 digest" <| do
      let
        field = Sql.Field
          (Sql.SQLText
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          )
          0
        sha256 = Sha256.fromDigest <| Crypto.hash ByteString.empty
      Sql.fromField field `shouldBe` pure sha256

    it "fails to parse an invalid SHA-256 digest" <| do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `shouldBe` (Sql.Errors [] :: Sql.Ok Sha256.Sha256)

  describe "toField" <| do

    it "renders a SHA-256 digest" <| do
      let
        sha256 = Sha256.fromDigest <| Crypto.hash ByteString.empty
        sqlData = Sql.SQLText
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      Sql.toField sha256 `shouldBe` sqlData
