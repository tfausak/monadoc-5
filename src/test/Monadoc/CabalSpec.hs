module Monadoc.CabalSpec
  ( spec
  )
where

import qualified Data.Either as Either
import qualified Data.ByteString.Char8 as ByteString
import qualified Monadoc.Cabal as Monadoc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.it "parses an empty package" $ do
    Monadoc.parse (ByteString.pack "name:x\nversion:0") `Hspec.shouldSatisfy` Either.isRight

  Hspec.it "fails to parse an invalid package" $ do
    Monadoc.parse ByteString.empty `Hspec.shouldSatisfy` Either.isLeft
