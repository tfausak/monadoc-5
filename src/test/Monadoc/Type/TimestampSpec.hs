module Monadoc.Type.TimestampSpec where

import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Vendor.Time as Time
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Timestamp" $ do

  Test.describe "fromField" $ do

    Test.it "parses a timestamp" $ do
      let
        field = Sql.Field (Sql.SQLText "2001-02-03T04:05:06.007Z") 0
        timestamp = Timestamp.fromUtcTime $ Time.utcTime 2001 2 3 4 5 6.007
      Sql.fromField field `Test.shouldBe` pure timestamp

    Test.it "fails to parse an invalid timestamp" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field
        `Test.shouldBe` (Sql.Errors [] :: Sql.Ok Timestamp.Timestamp)

  Test.describe "toField" $ do

    Test.it "parses a timestamp" $ do
      let
        timestamp = Timestamp.fromUtcTime $ Time.utcTime 2001 2 3 4 5 6.007
        sqlData = Sql.SQLText "2001-02-03T04:05:06.007Z"
      Sql.toField timestamp `Test.shouldBe` sqlData
