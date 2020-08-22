module Monadoc.Utility.SqlSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Monadoc.Utility.Sql as Sql
import Test.Hspec
import qualified Text.Read as Read

spec :: Spec
spec = describe "Monadoc.Utility.Sql" $ do

  describe "fromFieldVia" $ do

    it "handles success" $ do
      let field = Sql.Field (Sql.SQLText "()") 0
      Sql.fromFieldVia Read.readMaybe field `shouldBe` Sql.Ok ()

    it "handles failure" $ do
      let field = Sql.Field (Sql.SQLText "not valid") 0
      Sql.fromFieldVia Read.readMaybe field
        `shouldBe` (Sql.Errors [] :: Sql.Ok ())
