module Monadoc.CommitSpec
  ( spec
  )
where

import qualified Monadoc.Commit as Monadoc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "hash" $ do

    Hspec.it "is not null if set" $ do
      case Monadoc.hash of
        Nothing -> pure ()
        Just hash -> hash `Hspec.shouldSatisfy` not . null
