module Monadoc.Extra.Time where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time

-- | Uses a format string to format a time value. Uses the
-- 'Time.defaultTimeLocale'.
format :: Time.FormatTime t => String -> t -> String
format = Time.formatTime Time.defaultTimeLocale

-- | Uses a format string to parse a time string. Uses the
-- 'Time.defaultTimeLocale'.
parse :: Time.ParseTime t => String -> String -> Maybe t
parse = Time.parseTimeM False Time.defaultTimeLocale

-- | Builds a 'Time.UTCTime' using the given year, month, day, hour, minute,
-- and second. Date values that are out of bounds will be clamped. Time values
-- that are out of bounds will be left alone.
utcTime :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Time.UTCTime
utcTime year month day hour minute second = Time.UTCTime
  { Time.utctDay = Time.fromGregorian year month day
  , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
    { Time.todHour = hour
    , Time.todMin = minute
    , Time.todSec = second
    }
  }
