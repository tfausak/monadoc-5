module Monadoc.Utility.TimeSpec where

import qualified Data.Time as Time
import Monadoc.Prelude
import qualified Monadoc.Utility.Time as Time
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Utility.Time" $ do

  describe "format" $ do

    it "formats a UTC time" $ do
      Time.format "%Y %m %d %H %M %S %3Q" (Time.utcTime 2001 2 3 4 5 6.007)
        `shouldBe` "2001 02 03 04 05 06 .007"

  describe "parse" $ do

    it "parses a UTC time" $ do
      Time.parse "%Y %m %d %H %M %S %Q" "2001 02 03 04 05 06 .007"
        `shouldBe` Just (Time.utcTime 2001 2 3 4 5 6.007)

    it "returns nothing on failure" $ do
      Time.parse "%Y" "invalid" `shouldBe` (Nothing :: Maybe Time.UTCTime)

  describe "utcTime" $ do

    it "builds a UTC time" $ do
      Time.utcTime 2001 2 3 4 5 6.007 `shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 2 3
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 4
          , Time.todMin = 5
          , Time.todSec = 6.007
          }
        }

    it "clamps date values" $ do
      Time.utcTime 2001 13 32 0 0 0 `shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 12 31
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 0
          , Time.todMin = 0
          , Time.todSec = 0
          }
        }

    it "does not clamp time values" $ do
      Time.utcTime 2001 1 1 25 61 61 `shouldBe` Time.UTCTime
        { Time.utctDay = Time.fromGregorian 2001 1 1
        , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
          { Time.todHour = 25
          , Time.todMin = 61
          , Time.todSec = 61
          }
        }
