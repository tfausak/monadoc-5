module Monadoc.Type.PathSpec where

import qualified Monadoc.Type.Path as Path
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Type.Path" $ do

  Test.describe "fromFilePath" $ do

    Test.it "treats forward and backward slashes the same" $ do
      Path.fromFilePath "a/b" `Test.shouldBe` Path.fromFilePath "a\\b"

  Test.describe "toFilePath" $ do

    Test.it "uses forward slashes" $ do
      Path.toFilePath (Path.fromFilePath "a\\b") `Test.shouldBe` "a/b"
