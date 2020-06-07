module Monadoc.Type.Sha256Spec
  ( spec
  )
where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Sha256" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "parses a SHA-256 digest" $ do
      let
        field = Sql.Field
          (Sql.SQLText
            (Text.pack
              "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            )
          )
          0
        sha256 = Sha256.fromDigest $ Crypto.hash ByteString.empty
      Sql.fromField field `Hspec.shouldBe` pure sha256

    Hspec.it "fails to parse an invalid SHA-256 digest" $ do
      let field = Sql.Field (Sql.SQLText (Text.pack "not valid")) 0
      Sql.fromField field
        `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Sha256.Sha256)

  Hspec.describe "toField" $ do

    Hspec.it "renders a SHA-256 digest" $ do
      let
        sha256 = Sha256.fromDigest $ Crypto.hash ByteString.empty
        sqlData = Sql.SQLText $ Text.pack
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      Sql.toField sha256 `Hspec.shouldBe` sqlData
