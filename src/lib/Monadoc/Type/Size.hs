module Monadoc.Type.Size where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Test.Hspec as Hspec

-- | The size of something in bytes. Although this is backed by an 'Int', by
-- convention it is never negative. It uses an 'Int' because most functions
-- that produce a size return 'Int's. Use 'fromInt' and 'toInt' to do
-- conversions.
newtype Size
  = Size Int
  deriving (Eq, Show)

instance Sql.FromField Size where
  fromField = fmap fromInt . Sql.fromField

instance Sql.ToField Size where
  toField = Sql.toField . toInt

fromInt :: Int -> Size
fromInt = Size

toInt :: Size -> Int
toInt (Size int) = int

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Size" $ do

  Hspec.describe "fromField" $ do

    Hspec.it "converts from an integer" $ do
      let
        field = Sql.Field (Sql.SQLInteger 123) 0
        size = fromInt 123
      Sql.fromField field `Hspec.shouldBe` pure size

  Hspec.describe "toField" $ do

    Hspec.it "converts to an integer" $ do
      let
        size = fromInt 123
        sqlData = Sql.SQLInteger 123
      Sql.toField size `Hspec.shouldBe` sqlData
