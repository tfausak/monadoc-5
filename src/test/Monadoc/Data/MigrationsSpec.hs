module Monadoc.Data.MigrationsSpec
  ( spec
  )
where

import qualified Monadoc.Data.Migrations as Migrations
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Data.Migrations" $ do

  Test.describe "migrations" $ do

    Test.it "has at least one migration" $ do
      Migrations.migrations `Test.shouldSatisfy` not . null
