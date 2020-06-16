module Monadoc.Utility.Utf8
  ( fromString
  , fromText
  , toString
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

-- | Converts a string into a UTF-8 encoded byte string. See 'fromText'.
fromString :: String -> ByteString.ByteString
fromString = fromText . Text.pack

-- | Converts text into a UTF-8 encoded byte string.
fromText :: Text.Text -> ByteString.ByteString
fromText = Text.encodeUtf8

-- | Converts a byte string into a string, assuming that the bytes are UTF-8.
-- Any invalid bytes will be replaced with U+FFFD, the Unicode replacement
-- character.
toString :: ByteString.ByteString -> String
toString = Text.unpack . Text.decodeUtf8With Text.lenientDecode
