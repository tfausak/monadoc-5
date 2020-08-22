module Monadoc.Utility.Sql where

import qualified Data.Typeable as Typeable
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Test.Hspec as Hspec

-- | Converts from a SQL field into a value using the given function. This is
-- mostly used to avoid all the boilerplate.
fromFieldVia
  :: (Sql.FromField a, Show a, Typeable.Typeable b)
  => (a -> Maybe b)
  -> Sql.Field
  -> Sql.Ok b
fromFieldVia f x = do
  y <- Sql.fromField x
  case f y of
    Nothing ->
      Sql.returnError Sql.ConversionFailed x $ "failed to convert " <> show y
    Just z -> pure z

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Utility.Sql" $ do

  Hspec.describe "fromFieldVia" $ do

    Hspec.it "handles success" $ do
      let field = Sql.Field (Sql.SQLText "") 0
      fromFieldVia Just field `Hspec.shouldBe` Sql.Ok ("" :: String)

    Hspec.it "handles failure" $ do
      let
        field = Sql.Field (Sql.SQLText "") 0
        f :: String -> Maybe String
        f = const Nothing
      fromFieldVia f field `Hspec.shouldBe` Sql.Errors []
