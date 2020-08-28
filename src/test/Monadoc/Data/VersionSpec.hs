module Monadoc.Data.VersionSpec where

import qualified Data.Version as Version
import qualified Monadoc.Data.Version as Monadoc.Version
import Monadoc.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Data.Version" $ do

  describe "string" $ do

    it "is not null" $ do
      Monadoc.Version.string `shouldSatisfy` not . null

  describe "version" $ do

    it "has four branches" $ do
      Version.versionBranch Monadoc.Version.version
        `shouldSatisfy` (== 4)
        . length

    it "has no tags" $ do
      let Version.Version _ tags = Monadoc.Version.version
      tags `shouldSatisfy` null
