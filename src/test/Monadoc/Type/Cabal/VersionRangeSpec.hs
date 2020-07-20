module Monadoc.Type.Cabal.VersionRangeSpec where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Type.Cabal.VersionRange as VersionRange
import Test

spec :: Spec
spec = describe "Monadoc.Type.Cabal.VersionRange" $ do

  describe "fromString" $ do

    it "works" $ do
      VersionRange.fromString "> 0" `shouldSatisfy` Maybe.isJust

  describe "toString" $ do

    it "works" $ do
      let string = ">0"
      Just versionRange <- pure $ VersionRange.fromString string
      VersionRange.toString versionRange `shouldBe` string
