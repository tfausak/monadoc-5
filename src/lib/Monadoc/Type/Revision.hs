module Monadoc.Type.Revision where

newtype Revision
  = Revision Word
  deriving (Eq, Show)

fromWord :: Word -> Revision
fromWord = Revision

increment :: Revision -> Revision
increment = fromWord . (+ 1) . toWord

toString :: Revision -> String
toString = show . toWord

toWord :: Revision -> Word
toWord (Revision word) = word

zero :: Revision
zero = fromWord 0
