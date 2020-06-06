module Monadoc.GhcSpec
  ( spec
  )
where

import qualified Data.Either as Either
import qualified Data.ByteString.Char8 as ByteString
import qualified Monadoc.Ghc as Ghc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do

  Hspec.it "parses an empty module" $ do
    result <- Ghc.parse "" $ ByteString.pack "module M where"
    result `Hspec.shouldSatisfy` Either.isRight

  Hspec.it "fails to parse an invalid module" $ do
    result <- Ghc.parse "" $ ByteString.pack "module"
    result `Hspec.shouldSatisfy` Either.isLeft
