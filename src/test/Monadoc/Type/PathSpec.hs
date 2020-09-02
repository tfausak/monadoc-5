module Monadoc.Type.PathSpec where

import Monadoc.Prelude
import qualified Monadoc.Type.Path as Path
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Type.Path" <| do

  describe "fromFilePath" <| do

    it "treats forward and backward slashes the same" <| do
      Path.fromFilePath "a/b" `shouldBe` Path.fromFilePath "a\\b"

  describe "toFilePath" <| do

    it "uses forward slashes" <| do
      Path.toFilePath (Path.fromFilePath "a\\b") `shouldBe` "a/b"
