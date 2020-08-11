module Monadoc.Type.Cabal.Version where

import qualified Data.List as List
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Vendor.Sql as Sql

newtype Version = Version Cabal.Version deriving (Eq, Ord, Show)

instance Sql.ToField Version where
  toField = Sql.toField . toString

fromCabal :: Cabal.Version -> Version
fromCabal = Version

toCabal :: Version -> Cabal.Version
toCabal (Version cabal) = cabal

toString :: Version -> String
toString = List.intercalate "." . fmap show . Cabal.versionNumbers . toCabal
