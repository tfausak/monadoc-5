module Monadoc.Type.Timestamp
  ( Timestamp
  , fromUtcTime
  , toUtcTime
  )
where

import qualified Data.Time as Time
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql

-- | A moment in time. This is a wrapper around 'Time.UTCTime'. Use
-- 'fromUtcTime' and 'toUtcTime' to wrap and unwrap these values. Since this
-- only uses UTC time, be careful with time zones when converting!
newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Ord, Show)

instance Sql.FromField Timestamp where
  fromField field = do
    string <- Sql.fromField field
    case parseTime "%Y-%m-%dT%H:%M:%S%QZ" string of
      Nothing ->
        Sql.returnError Sql.ConversionFailed field
          $ "invalid Timestamp: "
          <> show string
      Just utcTime -> pure $ fromUtcTime utcTime

instance Sql.ToField Timestamp where
  toField = Sql.toField . formatTime "%Y-%m-%dT%H:%M:%S%3QZ" . toUtcTime

fromUtcTime :: Time.UTCTime -> Timestamp
fromUtcTime = Timestamp

toUtcTime :: Timestamp -> Time.UTCTime
toUtcTime (Timestamp utcTime) = utcTime

formatTime :: Time.FormatTime t => String -> t -> String
formatTime = Time.formatTime Time.defaultTimeLocale

parseTime :: Time.ParseTime t => String -> String -> Maybe t
parseTime = Time.parseTimeM False Time.defaultTimeLocale
