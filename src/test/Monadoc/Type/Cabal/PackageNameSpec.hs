module Monadoc.Type.Cabal.PackageNameSpec
  ( spec
  )
where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Cabal.PackageName" $ do

  Test.describe "fromString" $ do

    Test.it "works" $ do
      PackageName.fromString "some-package" `Test.shouldSatisfy` Maybe.isJust

  Test.describe "toString" $ do

    Test.it "works" $ do
      let string = "some-package"
      Just packageName <- pure $ PackageName.fromString string
      PackageName.toString packageName `Test.shouldBe` string
