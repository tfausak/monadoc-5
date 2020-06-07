module Monadoc.CabalSpec
  ( spec
  )
where

import qualified Data.Either as Either
import qualified Data.ByteString.Char8 as ByteString
import qualified Monadoc.Cabal as Cabal
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Cabal" $ do

  Hspec.describe "parse" $ do

    Hspec.it "parses an empty package" $ do
      Cabal.parse (ByteString.pack "name:x\nversion:0")
        `Hspec.shouldSatisfy` Either.isRight

    Hspec.it "fails to parse an invalid package" $ do
      Cabal.parse ByteString.empty `Hspec.shouldSatisfy` Either.isLeft
