module Monadoc.Utility.Utf8 where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Monadoc.Prelude

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
