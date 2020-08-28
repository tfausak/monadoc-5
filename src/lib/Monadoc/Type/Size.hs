module Monadoc.Type.Size where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude

-- | The size of something in bytes. Although this is backed by an 'Int', by
-- convention it is never negative. It uses an 'Int' because most functions
-- that produce a size return 'Int's. Use 'fromInt' and 'toInt' to do
-- conversions.
newtype Size
  = Size Int
  deriving (Eq, Show)

instance Sql.FromField Size where
  fromField = fmap fromInt <<< Sql.fromField

instance Sql.ToField Size where
  toField = Sql.toField <<< toInt

fromInt :: Int -> Size
fromInt = Size

toInt :: Size -> Int
toInt (Size int) = int
