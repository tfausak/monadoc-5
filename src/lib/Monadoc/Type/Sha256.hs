module Monadoc.Type.Sha256
  ( Sha256
  , fromDigest
  , toDigest
  )
where

import qualified Crypto.Hash as Crypto
import qualified Monadoc.Vendor.Sql as Sql
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
  fromField field = do
    string <- Sql.fromField field
    case Read.readMaybe string of
      Nothing ->
        Sql.returnError Sql.ConversionFailed field
          $ "invalid Sha256: "
          <> show string
      Just digest -> pure $ fromDigest digest

instance Sql.ToField Sha256 where
  toField = Sql.toField . show . toDigest

fromDigest :: Crypto.Digest Crypto.SHA256 -> Sha256
fromDigest = Sha256

toDigest :: Sha256 -> Crypto.Digest Crypto.SHA256
toDigest (Sha256 digest) = digest
