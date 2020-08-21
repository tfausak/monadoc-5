module Monadoc.Utility.Utf8 where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Test.Hspec as Hspec

-- | Converts a string into a UTF-8 encoded byte string. See 'fromText'.
fromString :: String -> ByteString.ByteString
fromString = fromText . Text.pack

-- | Converts text into a UTF-8 encoded byte string.
fromText :: Text.Text -> ByteString.ByteString
fromText = Text.encodeUtf8

-- | Converts a UTF-8 encoded byte string into a string. See 'toText'.
toString :: ByteString.ByteString -> String
toString = Text.unpack . toText

-- | Converts a UTF-8 byte string into text, assuming that the bytes are UTF-8.
-- Any invalid bytes will be replaced with U+FFFD, the Unicode replacement
-- character.
toText :: ByteString.ByteString -> Text.Text
toText = Text.decodeUtf8With Text.lenientDecode

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Utility.Utf8" $ do

  Hspec.describe "fromString" $ do

    Hspec.it "encodes UTF-8" $ do
      fromString "$" `Hspec.shouldBe` ByteString.pack [0x24]
      fromString "\xa2" `Hspec.shouldBe` ByteString.pack [0xc2, 0xa2]
      fromString "\x20ac" `Hspec.shouldBe` ByteString.pack [0xe2, 0x82, 0xac]
      fromString "\x10348"
        `Hspec.shouldBe` ByteString.pack [0xf0, 0x90, 0x8d, 0x88]

  Hspec.describe "toString " $ do

    Hspec.it "decodes UTF-8" $ do
      toString (ByteString.pack [0x24]) `Hspec.shouldBe` "$"
      toString (ByteString.pack [0xc2, 0xa2]) `Hspec.shouldBe` "\xa2"
      toString (ByteString.pack [0xe2, 0x82, 0xac]) `Hspec.shouldBe` "\x20ac"
      toString (ByteString.pack [0xf0, 0x90, 0x8d, 0x88])
        `Hspec.shouldBe` "\x10348"

    Hspec.it "replaces invalid bytes" $ do
      toString (ByteString.pack [0xc0]) `Hspec.shouldBe` "\xfffd"
