module Monadoc.Type.Migration
  ( Migration(..)
  , sha256
  )
where

import qualified Crypto.Hash as Crypto
import qualified Data.Ord as Ord
import qualified Data.Text.Encoding as Text
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Sqlite as Sql

-- | A database migration. This is a single SQL statement along with a
-- timestamp. The timestamp is used both to order the migrations and as a
-- unique key for identifying them.
data Migration = Migration
  { query :: Sql.Query
  , timestamp :: Timestamp.Timestamp
  } deriving (Eq, Show)

instance Ord Migration where
  compare x y = Ord.comparing timestamp x y <> Ord.comparing query x y

instance Sql.ToRow Migration where
  toRow migration =
    [ Sql.toField $ timestamp migration
    , Sql.toField $ sha256 migration
    ]

-- | Computes a digest of the 'Migration' 'query'. This is used to make sure
-- that the migration hasn't changed since it was ran.
sha256 :: Migration -> Sha256.Sha256
sha256 =
  Sha256.fromDigest . Crypto.hash . Text.encodeUtf8 . Sql.fromQuery . query
