module Monadoc.Type.Sha256 where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec
import qualified Text.Read as Read

-- | A 256-bit digest from the Secure Hash Algorithm 2 (SHA-2). This is backed
-- by a 'Crypto.Digest', specifically 'Crypto.SHA256'. Use 'fromDigest' and
-- 'toDigest' to convert to and from this type. This algorithm was selected as
-- a good balance between fast methods like MD5 and cryptographically secure
-- ones like SHA3-512.
newtype Sha256
  = Sha256 (Crypto.Digest Crypto.SHA256)
  deriving (Eq, Show)

instance Sql.FromField Sha256 where
  fromField = Sql.fromFieldVia fromString

instance Sql.ToField Sha256 where
  toField = Sql.toField . toString

fromDigest :: Crypto.Digest Crypto.SHA256 -> Sha256
fromDigest = Sha256

fromString :: String -> Maybe Sha256
fromString = fmap fromDigest . Read.readMaybe

toDigest :: Sha256 -> Crypto.Digest Crypto.SHA256
toDigest (Sha256 digest) = digest

toString :: Sha256 -> String
toString = show . toDigest

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Sha256" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "parses a SHA-256 digest" $ do
      let
        field = Sql.Field
          (Sql.SQLText
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          )
          0
        sha256 = fromDigest $ Crypto.hash ByteString.empty
      Sql.fromField field `Hspec.shouldBe` pure sha256

    Hspec.it "fails to parse an invalid SHA-256 digest" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Sha256)

  Hspec.describe "toField" $ do

    Hspec.it "renders a SHA-256 digest" $ do
      let
        sha256 = fromDigest $ Crypto.hash ByteString.empty
        sqlData = Sql.SQLText
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      Sql.toField sha256 `Hspec.shouldBe` sqlData
