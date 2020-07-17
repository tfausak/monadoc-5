module Monadoc.Type.Cabal.PackageName where

import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Monadoc.Vendor.Sql as Sql

newtype PackageName
  = PackageName Cabal.PackageName
  deriving (Eq, Ord, Show)

instance Sql.ToField PackageName where
  toField = Sql.toField . toString

fromCabal :: Cabal.PackageName -> PackageName
fromCabal = PackageName

fromString :: String -> Maybe PackageName
fromString = fmap fromCabal . Cabal.simpleParsec

toCabal :: PackageName -> Cabal.PackageName
toCabal (PackageName cabal) = cabal

toString :: PackageName -> String
toString = Cabal.prettyShow . toCabal
