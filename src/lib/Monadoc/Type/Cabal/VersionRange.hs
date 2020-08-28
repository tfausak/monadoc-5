module Monadoc.Type.Cabal.VersionRange where

import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import Monadoc.Prelude

newtype VersionRange
  = VersionRange Cabal.VersionRange
  deriving (Eq, Show)

instance Sql.ToField VersionRange where
  toField = Sql.toField . toString

fromCabal :: Cabal.VersionRange -> VersionRange
fromCabal = VersionRange

fromString :: String -> Maybe VersionRange
fromString = fmap fromCabal . Cabal.simpleParsec

toCabal :: VersionRange -> Cabal.VersionRange
toCabal (VersionRange cabal) = cabal

toString :: VersionRange -> String
toString = Cabal.prettyShow . toCabal
