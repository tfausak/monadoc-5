module Monadoc.Type.Cabal.VersionRangeSpec where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Type.Cabal.VersionRange as VersionRange
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Cabal.VersionRange" $ do

  Test.describe "fromString" $ do

    Test.it "works" $ do
      VersionRange.fromString "> 0" `Test.shouldSatisfy` Maybe.isJust

  Test.describe "toString" $ do

    Test.it "works" $ do
      let string = ">0"
      Just versionRange <- pure $ VersionRange.fromString string
      VersionRange.toString versionRange `Test.shouldBe` string
