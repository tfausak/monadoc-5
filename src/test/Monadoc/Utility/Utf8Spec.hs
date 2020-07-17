module Monadoc.Utility.Utf8Spec where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Utility.Utf8" $ do

  Test.describe "fromString" $ do

    Test.it "encodes UTF-8" $ do
      Utf8.fromString "$" `Test.shouldBe` ByteString.pack [0x24]
      Utf8.fromString "\xa2" `Test.shouldBe` ByteString.pack [0xc2, 0xa2]
      Utf8.fromString "\x20ac"
        `Test.shouldBe` ByteString.pack [0xe2, 0x82, 0xac]
      Utf8.fromString "\x10348"
        `Test.shouldBe` ByteString.pack [0xf0, 0x90, 0x8d, 0x88]

  Test.describe "toString " $ do

    Test.it "decodes UTF-8" $ do
      Utf8.toString (ByteString.pack [0x24]) `Test.shouldBe` "$"
      Utf8.toString (ByteString.pack [0xc2, 0xa2]) `Test.shouldBe` "\xa2"
      Utf8.toString (ByteString.pack [0xe2, 0x82, 0xac])
        `Test.shouldBe` "\x20ac"
      Utf8.toString (ByteString.pack [0xf0, 0x90, 0x8d, 0x88])
        `Test.shouldBe` "\x10348"

    Test.it "replaces invalid bytes" $ do
      Utf8.toString (ByteString.pack [0xc0]) `Test.shouldBe` "\xfffd"
