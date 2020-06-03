{-# LANGUAGE OverloadedStrings #-}

module Monadoc.CabalSpec
  ( spec
  )
where

import qualified Data.Either as Either
import qualified Monadoc.Cabal as Monadoc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.it "parses an empty package"
    $ Monadoc.parse "name:x\nversion:0"
    `Hspec.shouldSatisfy` Either.isRight

  Hspec.it "fails to parse an invalid package"
    $ Monadoc.parse ""
    `Hspec.shouldSatisfy` Either.isLeft
