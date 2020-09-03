module Monadoc.Type.Cabal.Version where

import qualified Data.List as List
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.Version as Cabal
import Monadoc.Prelude hiding (fromString)
import qualified Monadoc.Utility.Sql as Sql

newtype Version = Version Cabal.Version deriving (Eq, Ord, Show)

instance Sql.FromField Version where
  fromField = Sql.fromFieldVia fromString

instance Sql.ToField Version where
  toField = Sql.toField <<< toString

fromCabal :: Cabal.Version -> Version
fromCabal = Version

fromString :: String -> Maybe Version
fromString = map fromCabal <<< Cabal.simpleParsec

toCabal :: Version -> Cabal.Version
toCabal (Version cabal) = cabal

toString :: Version -> String
toString =
  List.intercalate "." <<< map show <<< Cabal.versionNumbers <<< toCabal
