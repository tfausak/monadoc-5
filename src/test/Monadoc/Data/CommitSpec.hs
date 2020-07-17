module Monadoc.Data.CommitSpec where

import qualified Monadoc.Data.Commit as Commit
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Data.Commit" $ do

  Test.describe "hash" $ do

    Test.it "is not null if set" $ do
      case Commit.hash of
        Nothing -> pure ()
        Just hash -> hash `Test.shouldSatisfy` not . null
