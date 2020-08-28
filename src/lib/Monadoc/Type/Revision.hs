module Monadoc.Type.Revision where

import qualified Database.SQLite.Simple.ToField as Sql
import qualified Text.Read as Read
import Monadoc.Prelude

newtype Revision
  = Revision Word
  deriving (Eq, Show)

instance Sql.ToField Revision where
  toField = Sql.toField . toWord

fromString :: String -> Maybe Revision
fromString = fmap Revision . Read.readMaybe

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
