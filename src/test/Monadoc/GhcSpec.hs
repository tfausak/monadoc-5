module Monadoc.GhcSpec
  ( spec
  )
where

import qualified Data.Either as Either
import qualified Monadoc.Ghc as Ghc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Ghc" $ do

  Hspec.describe "parse" $ do

    Hspec.it "parses an empty module" $ do
      result <- Ghc.parse "" "module M where"
      result `Hspec.shouldSatisfy` Either.isRight

    Hspec.it "fails to parse an invalid module" $ do
      result <- Ghc.parse "" "module"
      result `Hspec.shouldSatisfy` Either.isLeft
