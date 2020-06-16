module Monadoc.Vendor.TimeSpec
  ( spec
  )
where

import qualified Monadoc.Vendor.Time as Time
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Vendor.Time" $ do

  Test.describe "formatTime" $ do

    Test.it "formats a UTC time" $ do
      Time.formatTime "%Y %m %d %H %M %S %3Q" (Time.utcTime 2001 2 3 4 5 6.007)
        `Test.shouldBe` "2001 02 03 04 05 06 .007"

  Test.describe "parseTime" $ do

    Test.it "parses a UTC time" $ do
      Time.parseTime "%Y %m %d %H %M %S %Q" "2001 02 03 04 05 06 .007"
        `Test.shouldBe` Just (Time.utcTime 2001 2 3 4 5 6.007)

    Test.it "returns nothing on failure" $ do
      Time.parseTime "%Y" "invalid"
        `Test.shouldBe` (Nothing :: Maybe Time.UTCTime)

  Test.describe "utcTime" $ do

    Test.it "builds a UTC time" $ do
      Time.utcTime 2001 2 3 4 5 6.007 `Test.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 2 3
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 4
          , Time.todMin = 5
          , Time.todSec = 6.007
          }
        }

    Test.it "clamps date values" $ do
      Time.utcTime 2001 13 32 0 0 0 `Test.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 12 31
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 0
          , Time.todMin = 0
          , Time.todSec = 0
          }
        }

    Test.it "does not clamp time values" $ do
      Time.utcTime 2001 1 1 25 61 61 `Test.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 1 1
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 25
          , Time.todMin = 61
          , Time.todSec = 61
          }
        }
