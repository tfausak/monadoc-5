module Monadoc.Data.Commit where

import qualified Test.Hspec as Hspec

-- | The Git commit hash this package was built at, if available. Like the
-- version number, you'll probably only need this for diagnostics.
hash :: Maybe String
hash =
  -- This looks pretty silly by itself. See src/script/set-commit-hash.hs for
  -- an explanation.
  Nothing

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Data.Commit" $ do

  Hspec.describe "hash" $ do

    Hspec.it "is not null if set" $ do
      hash `Hspec.shouldSatisfy` maybe True (not . null)
