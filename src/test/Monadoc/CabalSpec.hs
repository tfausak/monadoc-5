module Monadoc.CabalSpec where

import qualified Data.Either as Either
import qualified Monadoc.Cabal as Cabal
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Cabal" $ do

  Test.describe "parse" $ do

    Test.it "parses an empty package" $ do
      Cabal.parse "name:x\nversion:0" `Test.shouldSatisfy` Either.isRight

    Test.it "fails to parse an invalid package" $ do
      Cabal.parse "" `Test.shouldSatisfy` Either.isLeft
