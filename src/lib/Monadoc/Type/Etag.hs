module Monadoc.Type.Etag
  ( Etag
  , fromByteString
  , toByteString
  )
where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Vendor.Sqlite as Sql
import qualified Text.Read as Read

-- | An HTTP entity tag (ETag). Values typically come from HTTP headers, so
-- this is a 'ByteString.ByteString' behind the scenes. Usually values are
-- quoted ASCII strings like @"01ef"@. Use 'fromByteString' and 'toByteString'
-- for conversion.
newtype Etag
  = Etag ByteString.ByteString
  deriving (Eq, Show)

instance Sql.FromField Etag where
  fromField field = do
    string <- Sql.fromField field
    case Read.readMaybe string of
      Nothing ->
        Sql.returnError Sql.ConversionFailed field
          $ "invalid Etag: "
          <> show string
      Just byteString -> pure $ fromByteString byteString

instance Sql.ToField Etag where
  toField = Sql.toField . show . toByteString

fromByteString :: ByteString.ByteString -> Etag
fromByteString = Etag

toByteString :: Etag -> ByteString.ByteString
toByteString (Etag byteString) = byteString
