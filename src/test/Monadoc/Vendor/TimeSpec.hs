module Monadoc.Vendor.TimeSpec
  ( spec
  )
where

import qualified Monadoc.Vendor.Time as Time
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Vendor.Time" $ do

  Hspec.describe "formatTime" $ do

    Hspec.it "formats a UTC time" $ do
      Time.formatTime "%Y %m %d %H %M %S %3Q" (Time.utcTime 2001 2 3 4 5 6.007)
        `Hspec.shouldBe` "2001 02 03 04 05 06 .007"

  Hspec.describe "parseTime" $ do

    Hspec.it "parses a UTC time" $ do
      Time.parseTime "%Y %m %d %H %M %S %Q" "2001 02 03 04 05 06 .007"
        `Hspec.shouldBe` Just (Time.utcTime 2001 2 3 4 5 6.007)

    Hspec.it "returns nothing on failure" $ do
      Time.parseTime "%Y" "invalid"
        `Hspec.shouldBe` (Nothing :: Maybe Time.UTCTime)

  Hspec.describe "utcTime" $ do

    Hspec.it "builds a UTC time" $ do
      Time.utcTime 2001 2 3 4 5 6.007 `Hspec.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 2 3
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 4
          , Time.todMin = 5
          , Time.todSec = 6.007
          }
        }

    Hspec.it "clamps date values" $ do
      Time.utcTime 2001 13 32 0 0 0 `Hspec.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 12 31
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 0
          , Time.todMin = 0
          , Time.todSec = 0
          }
        }

    Hspec.it "does not clamp time values" $ do
      Time.utcTime 2001 1 1 25 61 61 `Hspec.shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 1 1
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 25
          , Time.todMin = 61
          , Time.todSec = 61
          }
        }
