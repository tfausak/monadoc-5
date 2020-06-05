module Monadoc.Vendor.Time
  ( Data.Time.FormatTime
  , Data.Time.NominalDiffTime
  , Data.Time.ParseTime
  , Data.Time.TimeOfDay(..)
  , Data.Time.UTCTime(..)
  , formatTime
  , Data.Time.fromGregorian
  , Data.Time.getCurrentTime
  , parseTime
  , Data.Time.Clock.POSIX.posixSecondsToUTCTime
  , Data.Time.timeOfDayToTime
  , utcTime
  )
where

import qualified Data.Fixed as Fixed
import qualified Data.Time
import qualified Data.Time.Clock.POSIX

-- | Uses a format string to format a time value. Uses the
-- 'Data.Time.defaultTimeLocale'.
formatTime :: Data.Time.FormatTime t => String -> t -> String
formatTime = Data.Time.formatTime Data.Time.defaultTimeLocale

-- | Uses a format string to parse a time string. Uses the
-- 'Data.Time.defaultTimeLocale'.
parseTime :: Data.Time.ParseTime t => String -> String -> Maybe t
parseTime = Data.Time.parseTimeM False Data.Time.defaultTimeLocale

-- | Builds a 'Data.Time.UTCTime' using the given year, month, day, hour,
-- minute, and second. Date values that are out of bounds will be clamped.
-- Time values that are out of bounds will be left alone.
utcTime :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Data.Time.UTCTime
utcTime year month day hour minute second = Data.Time.UTCTime
  { Data.Time.utctDay = Data.Time.fromGregorian year month day
  , Data.Time.utctDayTime = Data.Time.timeOfDayToTime Data.Time.TimeOfDay
    { Data.Time.todHour = hour
    , Data.Time.todMin = minute
    , Data.Time.todSec = second
    }
  }
