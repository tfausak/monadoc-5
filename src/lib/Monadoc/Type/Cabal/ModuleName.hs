module Monadoc.Type.Cabal.ModuleName where

import qualified Data.List as List
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.ModuleName as Cabal

newtype ModuleName = ModuleName Cabal.ModuleName deriving (Eq, Ord, Show)

instance Sql.ToField ModuleName where
  toField = Sql.toField . toString

fromCabal :: Cabal.ModuleName -> ModuleName
fromCabal = ModuleName

fromString :: String -> ModuleName
fromString = fromCabal . Cabal.fromString

fromStrings :: [String] -> ModuleName
fromStrings = fromCabal . Cabal.fromComponents

toCabal :: ModuleName -> Cabal.ModuleName
toCabal (ModuleName cabal) = cabal

toString :: ModuleName -> String
toString = List.intercalate "." . toStrings

toStrings :: ModuleName -> [String]
toStrings = Cabal.components . toCabal
