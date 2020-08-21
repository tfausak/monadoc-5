module Monadoc.Extra.Time where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Test.Hspec as Hspec

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

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.Time" $ do

  Hspec.describe "format" $ do

    Hspec.it "formats a UTC time" $ do
      format "%Y %m %d %H %M %S %3Q" (utcTime 2001 2 3 4 5 6.007)
        `Hspec.shouldBe` "2001 02 03 04 05 06 .007"

  Hspec.describe "parse" $ do

    Hspec.it "parses a UTC time" $ do
      parse "%Y %m %d %H %M %S %Q" "2001 02 03 04 05 06 .007"
        `Hspec.shouldBe` Just (utcTime 2001 2 3 4 5 6.007)

    Hspec.it "returns nothing on failure" $ do
      parse "%Y" "invalid" `Hspec.shouldBe` (Nothing :: Maybe Time.UTCTime)

  Hspec.describe "utcTime" $ do

    Hspec.it "builds a UTC time" $ do
      utcTime 2001 2 3 4 5 6.007 `Hspec.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 2 3
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 4
          , Time.todMin = 5
          , Time.todSec = 6.007
          }
        }

    Hspec.it "clamps date values" $ do
      utcTime 2001 13 32 0 0 0 `Hspec.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 12 31
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 0
          , Time.todMin = 0
          , Time.todSec = 0
          }
        }

    Hspec.it "does not clamp time values" $ do
      utcTime 2001 1 1 25 61 61 `Hspec.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 1 1
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 25
          , Time.todMin = 61
          , Time.todSec = 61
          }
        }
