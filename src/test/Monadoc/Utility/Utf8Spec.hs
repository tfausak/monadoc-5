module Monadoc.Utility.Utf8Spec where

import qualified Data.ByteString as ByteString
import Monadoc.Prelude
import qualified Monadoc.Utility.Utf8 as Utf8
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Utility.Utf8" $ do

  describe "fromString" $ do

    it "encodes UTF-8" $ do
      Utf8.fromString "$" `shouldBe` ByteString.pack [0x24]
      Utf8.fromString "\xa2" `shouldBe` ByteString.pack [0xc2, 0xa2]
      Utf8.fromString "\x20ac" `shouldBe` ByteString.pack [0xe2, 0x82, 0xac]
      Utf8.fromString "\x10348"
        `shouldBe` ByteString.pack [0xf0, 0x90, 0x8d, 0x88]

  describe "toString " $ do

    it "decodes UTF-8" $ do
      Utf8.toString (ByteString.pack [0x24]) `shouldBe` "$"
      Utf8.toString (ByteString.pack [0xc2, 0xa2]) `shouldBe` "\xa2"
      Utf8.toString (ByteString.pack [0xe2, 0x82, 0xac]) `shouldBe` "\x20ac"
      Utf8.toString (ByteString.pack [0xf0, 0x90, 0x8d, 0x88])
        `shouldBe` "\x10348"

    it "replaces invalid bytes" $ do
      Utf8.toString (ByteString.pack [0xc0]) `shouldBe` "\xfffd"
