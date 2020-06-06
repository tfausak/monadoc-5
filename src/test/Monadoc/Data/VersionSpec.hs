module Monadoc.Data.VersionSpec
  ( spec
  )
where

import qualified Data.Version as Version
import qualified Monadoc.Data.Version as Monadoc.Version
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.describe "string" $ do

    Hspec.it "is not null" $ do
      Monadoc.Version.string `Hspec.shouldSatisfy` not . null

  Hspec.describe "version" $ do

    Hspec.it "has four branches" $ do
      Version.versionBranch Monadoc.Version.version
        `Hspec.shouldSatisfy` (== 4)
        . length

    Hspec.it "has no tags" $ do
      let Version.Version _ tags = Monadoc.Version.version
      tags `Hspec.shouldSatisfy` null
