module Monadoc.Type.SizeSpec
  ( spec
  )
where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Size as Monadoc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "fromField" $ do

    Hspec.it "converts from an integer" $ do
      let
        field = Sql.Field (Sql.SQLInteger 123) 0
        size = Monadoc.fromInt 123
      Sql.fromField field `Hspec.shouldBe` pure size

  Hspec.describe "toField" $ do

    Hspec.it "converts to an integer" $ do
      let
        size = Monadoc.fromInt 123
        sqlData = Sql.SQLInteger 123
      Sql.toField size `Hspec.shouldBe` sqlData
