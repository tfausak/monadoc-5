module Monadoc.Type.Cabal.ModuleName where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.ModuleName as Cabal
import Monadoc.Prelude
import qualified Monadoc.Utility.Sql as Sql

newtype ModuleName
  = ModuleName Cabal.ModuleName
  deriving (Eq, Ord, Show)

instance Sql.FromField ModuleName where
  fromField = Sql.fromFieldVia fromText

instance Sql.ToField ModuleName where
  toField = Sql.toField <<< toString

fromCabal :: Cabal.ModuleName -> ModuleName
fromCabal = ModuleName

fromString :: String -> ModuleName
fromString = fromCabal <<< Cabal.fromString

fromText :: Text -> Maybe ModuleName
fromText = Text.unpack >>> fromString >>> Just

fromStrings :: [String] -> ModuleName
fromStrings = fromCabal <<< Cabal.fromComponents

toCabal :: ModuleName -> Cabal.ModuleName
toCabal (ModuleName cabal) = cabal

toString :: ModuleName -> String
toString = List.intercalate "." <<< toStrings

toText :: ModuleName -> Text
toText = toString >>> Text.pack

toStrings :: ModuleName -> [String]
toStrings = Cabal.components <<< toCabal
