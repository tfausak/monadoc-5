module Monadoc.Type.Cabal.VersionRange
  ( VersionRange
  , fromCabal
  , fromString
  , toCabal
  , toString
  )
where

import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified Monadoc.Vendor.Sql as Sql

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
