module Monadoc.Type.Timestamp where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Vendor.Time as Time

-- | A moment in time. This is a wrapper around 'Time.UTCTime'. Use
-- 'fromUtcTime' and 'toUtcTime' to wrap and unwrap these values. Since this
-- only uses UTC time, be careful with time zones when converting!
newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Ord, Show)

instance Sql.FromField Timestamp where
  fromField =
    Sql.fromFieldVia $ fmap fromUtcTime . Time.parse "%Y-%m-%dT%H:%M:%S%QZ"

instance Sql.ToField Timestamp where
  toField = Sql.toField . Time.format "%Y-%m-%dT%H:%M:%S%3QZ" . toUtcTime

fromUtcTime :: Time.UTCTime -> Timestamp
fromUtcTime = Timestamp

toUtcTime :: Timestamp -> Time.UTCTime
toUtcTime (Timestamp utcTime) = utcTime
