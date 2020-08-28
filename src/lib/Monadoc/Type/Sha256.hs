module Monadoc.Type.Sha256 where

import qualified Crypto.Hash as Crypto
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude hiding (fromString)
import qualified Monadoc.Utility.Sql as Sql
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
  toField = Sql.toField <<< toString

fromDigest :: Crypto.Digest Crypto.SHA256 -> Sha256
fromDigest = Sha256

fromString :: String -> Maybe Sha256
fromString = map fromDigest <<< Read.readMaybe

toDigest :: Sha256 -> Crypto.Digest Crypto.SHA256
toDigest (Sha256 digest) = digest

toString :: Sha256 -> String
toString = show <<< toDigest
