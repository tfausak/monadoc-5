module Monadoc.Type.Etag where

import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Utility.Sql as Sql
import qualified Text.Read as Read

-- | An HTTP entity tag (ETag). Values typically come from HTTP headers, so
-- this is a 'ByteString.ByteString' behind the scenes. Usually values are
-- quoted ASCII strings like @"01ef"@. Use 'fromByteString' and 'toByteString'
-- for conversion.
newtype Etag
  = Etag ByteString.ByteString
  deriving (Eq, Show)

instance Sql.FromField Etag where
  fromField = Sql.fromFieldVia $ fmap fromByteString . Read.readMaybe

instance Sql.ToField Etag where
  toField = Sql.toField . show . toByteString

fromByteString :: ByteString.ByteString -> Etag
fromByteString = Etag

toByteString :: Etag -> ByteString.ByteString
toByteString (Etag byteString) = byteString
