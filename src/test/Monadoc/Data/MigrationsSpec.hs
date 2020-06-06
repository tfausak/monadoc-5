module Monadoc.Data.MigrationsSpec
  ( spec
  )
where

import qualified Monadoc.Data.Migrations as Migrations
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "migrations" $ do

    Hspec.it "has at least one migration" $ do
      Migrations.migrations `Hspec.shouldSatisfy` not . null
