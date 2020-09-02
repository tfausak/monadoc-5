module Monadoc.Data.MigrationsSpec where

import qualified Monadoc.Data.Migrations as Migrations
import Monadoc.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Data.Migrations" <| do

  describe "migrations" <| do

    it "has at least one migration" <| do
      Migrations.migrations `shouldSatisfy` present
