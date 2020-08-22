module Monadoc.Type.Timestamp where

import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Utility.Sql as Sql
import qualified Monadoc.Utility.Time as Time
import qualified Test.Hspec as Hspec

-- | A moment in time. This is a wrapper around 'Time.UTCTime'. Use
-- 'fromUtcTime' and 'toUtcTime' to wrap and unwrap these values. Since this
-- only uses UTC time, be careful with time zones when converting!
newtype Timestamp
  = Timestamp Time.UTCTime
  deriving (Eq, Ord, Show)

instance Sql.FromField Timestamp where
  fromField =
    Sql.fromFieldVia $ fmap fromUtcTime . Time.parse "%Y-%m-%dT%H:%M:%S%QZ"

instance Sql.ToField Timestamp where
  toField = Sql.toField . Time.format "%Y-%m-%dT%H:%M:%S%3QZ" . toUtcTime

fromUtcTime :: Time.UTCTime -> Timestamp
fromUtcTime = Timestamp

toUtcTime :: Timestamp -> Time.UTCTime
toUtcTime (Timestamp utcTime) = utcTime

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Timestamp" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "parses a timestamp" $ do
      let
        field = Sql.Field (Sql.SQLText "2001-02-03T04:05:06.007Z") 0
        timestamp = fromUtcTime $ Time.utcTime 2001 2 3 4 5 6.007
      Sql.fromField field `Hspec.shouldBe` pure timestamp

    Hspec.it "fails to parse an invalid timestamp" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromField field `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok Timestamp)

  Hspec.describe "toField" $ do

    Hspec.it "parses a timestamp" $ do
      let
        timestamp = fromUtcTime $ Time.utcTime 2001 2 3 4 5 6.007
        sqlData = Sql.SQLText "2001-02-03T04:05:06.007Z"
      Sql.toField timestamp `Hspec.shouldBe` sqlData
