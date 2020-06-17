module Monadoc.Type.Sha256Spec
  ( spec
  )
where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Sha256" $ do

  Test.describe "fromField" $ do

    Test.it "parses a SHA-256 digest" $ do
      let
        field = Sql.Field
          (Sql.SQLText
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          )
          0
        sha256 = Sha256.fromDigest $ Crypto.hash ByteString.empty
      Sql.fromField field `Test.shouldBe` pure sha256

    Test.it "fails to parse an invalid SHA-256 digest" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field
        `Test.shouldBe` (Sql.Errors [] :: Sql.Ok Sha256.Sha256)

  Test.describe "toField" $ do

    Test.it "renders a SHA-256 digest" $ do
      let
        sha256 = Sha256.fromDigest $ Crypto.hash ByteString.empty
        sqlData = Sql.SQLText
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      Sql.toField sha256 `Test.shouldBe` sqlData
