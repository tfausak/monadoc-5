module Monadoc.Type.Timestamp
  ( Timestamp
  , fromUtcTime
  , toUtcTime
  )
where

import qualified Monadoc.Vendor.Sqlite as Sql
import qualified Monadoc.Vendor.Time as Time

-- | A moment in time. This is a wrapper around 'Time.UTCTime'. Use
-- 'fromUtcTime' and 'toUtcTime' to wrap and unwrap these values. Since this
-- only uses UTC time, be careful with time zones when converting!
newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Ord, Show)

instance Sql.FromField Timestamp where
  fromField field = do
    string <- Sql.fromField field
    case Time.parseTime "%Y-%m-%dT%H:%M:%S%QZ" string of
      Nothing ->
        Sql.returnError Sql.ConversionFailed field
          $ "invalid Timestamp: "
          <> show string
      Just utcTime -> pure $ fromUtcTime utcTime

instance Sql.ToField Timestamp where
  toField = Sql.toField . Time.formatTime "%Y-%m-%dT%H:%M:%S%3QZ" . toUtcTime

fromUtcTime :: Time.UTCTime -> Timestamp
fromUtcTime = Timestamp

toUtcTime :: Timestamp -> Time.UTCTime
toUtcTime (Timestamp utcTime) = utcTime
