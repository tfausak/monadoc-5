module Monadoc.Type.Cabal.PackageNameSpec where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import Test

spec :: Spec
spec = describe "Monadoc.Type.Cabal.PackageName" $ do

  describe "fromString" $ do

    it "works" $ do
      PackageName.fromString "some-package" `shouldSatisfy` Maybe.isJust

  describe "toString" $ do

    it "works" $ do
      let string = "some-package"
      Just packageName <- pure $ PackageName.fromString string
      PackageName.toString packageName `shouldBe` string
