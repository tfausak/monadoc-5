module Monadoc.Utility.Utf8Spec
  ( spec
  )
where

import qualified Data.ByteString as ByteString
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Utility.Utf8" $ do

  Hspec.describe "fromString" $ do

    Hspec.it "encodes UTF-8" $ do
      Utf8.fromString "$" `Hspec.shouldBe` ByteString.pack [0x24]
      Utf8.fromString "\xa2" `Hspec.shouldBe` ByteString.pack [0xc2, 0xa2]
      Utf8.fromString "\x20ac"
        `Hspec.shouldBe` ByteString.pack [0xe2, 0x82, 0xac]
      Utf8.fromString "\x10348"
        `Hspec.shouldBe` ByteString.pack [0xf0, 0x90, 0x8d, 0x88]

  Hspec.describe "toString " $ do

    Hspec.it "decodes UTF-8" $ do
      Utf8.toString (ByteString.pack [0x24]) `Hspec.shouldBe` "$"
      Utf8.toString (ByteString.pack [0xc2, 0xa2]) `Hspec.shouldBe` "\xa2"
      Utf8.toString (ByteString.pack [0xe2, 0x82, 0xac])
        `Hspec.shouldBe` "\x20ac"
      Utf8.toString (ByteString.pack [0xf0, 0x90, 0x8d, 0x88])
        `Hspec.shouldBe` "\x10348"

    Hspec.it "replaces invalid bytes" $ do
      Utf8.toString (ByteString.pack [0xc0]) `Hspec.shouldBe` "\xfffd"
