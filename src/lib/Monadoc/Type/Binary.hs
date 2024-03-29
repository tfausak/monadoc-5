module Monadoc.Type.Binary where

import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude

-- | Some binary data. Just a 'ByteString.ByteString' under the hood. Use
-- 'fromByteString' and 'toByteString' to work with these values.
newtype Binary
  = Binary ByteString.ByteString
  deriving (Eq, Show)

instance Sql.FromField Binary where
  fromField = map fromByteString <<< Sql.fromField

instance Sql.ToField Binary where
  toField = Sql.toField <<< toByteString

fromByteString :: ByteString.ByteString -> Binary
fromByteString = Binary

toByteString :: Binary -> ByteString.ByteString
toByteString (Binary byteString) = byteString
