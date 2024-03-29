module Monadoc.Type.Revision where

import qualified Database.SQLite.Simple.ToField as Sql
import Monadoc.Prelude
import qualified Text.Read as Read

newtype Revision
  = Revision Word
  deriving (Eq, Show)

instance Sql.ToField Revision where
  toField = Sql.toField <<< toWord

fromString :: String -> Maybe Revision
fromString = map Revision <<< Read.readMaybe

fromWord :: Word -> Revision
fromWord = Revision

increment :: Revision -> Revision
increment = fromWord <<< (+ 1) <<< toWord

toString :: Revision -> String
toString = show <<< toWord

toWord :: Revision -> Word
toWord (Revision word) = word

zero :: Revision
zero = fromWord 0
