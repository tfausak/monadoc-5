module Monadoc.Type.TimestampSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Utility.Time as Time
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Timestamp" <| do

  describe "fromField" <| do

    it "parses a timestamp" <| do
      let
        field = Sql.Field (Sql.SQLText "2001-02-03T04:05:06.007Z") 0
        timestamp = Timestamp.fromUtcTime <| Time.utcTime 2001 2 3 4 5 6.007
      Sql.fromField field `shouldBe` pure timestamp

    it "fails to parse an invalid timestamp" <| do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field
        `shouldBe` (Sql.Errors [] :: Sql.Ok Timestamp.Timestamp)

  describe "toField" <| do

    it "parses a timestamp" <| do
      let
        timestamp = Timestamp.fromUtcTime <| Time.utcTime 2001 2 3 4 5 6.007
        sqlData = Sql.SQLText "2001-02-03T04:05:06.007Z"
      Sql.toField timestamp `shouldBe` sqlData
