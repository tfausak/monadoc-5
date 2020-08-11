module Monadoc.Type.Cabal.ModuleName where

import qualified Data.List as List
import qualified Distribution.ModuleName as Cabal
import qualified Monadoc.Vendor.Sql as Sql

newtype ModuleName = ModuleName Cabal.ModuleName deriving (Eq, Show)

instance Sql.ToField ModuleName where
  toField = Sql.toField . toString

fromCabal :: Cabal.ModuleName -> ModuleName
fromCabal = ModuleName

toCabal :: ModuleName -> Cabal.ModuleName
toCabal (ModuleName cabal) = cabal

toString :: ModuleName -> String
toString = List.intercalate "." . Cabal.components . toCabal
