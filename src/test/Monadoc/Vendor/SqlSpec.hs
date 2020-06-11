module Monadoc.Vendor.SqlSpec
  ( spec
  )
where

import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec
import qualified Text.Read as Read

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Vendor.Sql" $ do

  Hspec.describe "fromFieldVia" $ do

    Hspec.it "handles success" $ do
      let field = Sql.Field (Sql.SQLText "()") 0
      Sql.fromFieldVia Read.readMaybe field `Hspec.shouldBe` Sql.Ok ()

    Hspec.it "handles failure" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromFieldVia Read.readMaybe field
        `Hspec.shouldBe` (Sql.Errors [] :: Sql.Ok ())
