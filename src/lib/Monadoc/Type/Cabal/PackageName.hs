module Monadoc.Type.Cabal.PackageName where

import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.PackageName as Cabal
import Monadoc.Prelude

newtype PackageName
  = PackageName Cabal.PackageName
  deriving (Eq, Ord, Show)

instance Sql.ToField PackageName where
  toField = Sql.toField <<< toString

fromCabal :: Cabal.PackageName -> PackageName
fromCabal = PackageName

fromString :: String -> Maybe PackageName
fromString = map fromCabal <<< Cabal.simpleParsec

toCabal :: PackageName -> Cabal.PackageName
toCabal (PackageName cabal) = cabal

toString :: PackageName -> String
toString = Cabal.prettyShow <<< toCabal
