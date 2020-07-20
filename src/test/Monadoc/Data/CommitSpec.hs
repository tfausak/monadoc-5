module Monadoc.Data.CommitSpec where

import qualified Monadoc.Data.Commit as Commit
import Test

spec :: Spec
spec = describe "Monadoc.Data.Commit" $ do

  describe "hash" $ do

    it "is not null if set" $ do
      case Commit.hash of
        Nothing -> pure ()
        Just hash -> hash `shouldSatisfy` not . null
