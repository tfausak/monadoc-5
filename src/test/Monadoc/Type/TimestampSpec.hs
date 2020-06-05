module Monadoc.Type.TimestampSpec
  ( spec
  )
where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Monadoc.Type.Timestamp as Monadoc
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "fromField" $ do

    Hspec.it "parses a timestamp" $ do
      let
        field =
          Sql.Field (Sql.SQLText (Text.pack "2001-02-03T04:05:06.007Z")) 0
        timestamp =
          Monadoc.fromUtcTime
            . Time.UTCTime (Time.fromGregorian 2001 2 3)
            . Time.timeOfDayToTime
            $ Time.TimeOfDay 4 5 6.007
      Sql.fromField field `Hspec.shouldBe` pure timestamp

    Hspec.it "fails to parse an invalid timestamp" $ do
      let field = Sql.Field (Sql.SQLText (Text.pack "not valid")) 0
      Sql.fromField field
        `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Monadoc.Timestamp)

  Hspec.describe "toField" $ do

    Hspec.it "parses a timestamp" $ do
      let
        timestamp =
          Monadoc.fromUtcTime
            . Time.UTCTime (Time.fromGregorian 2001 2 3)
            . Time.timeOfDayToTime
            $ Time.TimeOfDay 4 5 6.007
        sqlData = Sql.SQLText (Text.pack "2001-02-03T04:05:06.007Z")
      Sql.toField timestamp `Hspec.shouldBe` sqlData
