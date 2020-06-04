module Monadoc.Type.Sha256
  ( Sha256
  , fromDigest
  , toDigest
  )
where

import qualified Crypto.Hash as Crypto
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Text.Read as Read

-- | A 256-bit digest from the Secure Hash Algorithm 2 (SHA-2). This algorithm
-- was selected as a good balance between speed (like MD5) and cryptographic
-- properties (like SHA3-512).
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
