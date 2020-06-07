module Monadoc.Data.CommitSpec
  ( spec
  )
where

import qualified Monadoc.Data.Commit as Commit
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Data.Commit" $ do

  Hspec.describe "hash" $ do

    Hspec.it "is not null if set" $ do
      case Commit.hash of
        Nothing -> pure ()
        Just hash -> hash `Hspec.shouldSatisfy` not . null
