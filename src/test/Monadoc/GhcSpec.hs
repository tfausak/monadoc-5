{-# LANGUAGE OverloadedStrings #-}

module Monadoc.GhcSpec ( spec ) where

import qualified Data.Either as Either
import qualified Monadoc.Ghc as Monadoc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.it "parses an empty module" $ do
    result <- Monadoc.parse "" "module M where"
    result `Hspec.shouldSatisfy` Either.isRight

  Hspec.it "fails to parse an invalid module" $ do
    result <- Monadoc.parse "" "module"
    result `Hspec.shouldSatisfy` Either.isLeft
