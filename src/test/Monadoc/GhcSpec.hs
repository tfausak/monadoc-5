module Monadoc.GhcSpec
  ( spec
  )
where

import qualified Data.Either as Either
import qualified Monadoc.Ghc as Ghc
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Ghc" $ do

  Test.describe "parse" $ do

    Test.it "parses an empty module" $ do
      result <- Ghc.parse "" "module M where"
      result `Test.shouldSatisfy` Either.isRight

    Test.it "fails to parse an invalid module" $ do
      result <- Ghc.parse "" "module"
      result `Test.shouldSatisfy` Either.isLeft
