module Monadoc.Type.Cabal.PackageName where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.PackageName as Cabal
import Monadoc.Prelude hiding (fromString)
import qualified Monadoc.Utility.Sql as Sql

newtype PackageName
  = PackageName Cabal.PackageName
  deriving (Eq, Ord, Show)

instance Sql.FromField PackageName where
  fromField = Sql.fromFieldVia fromString

instance Sql.ToField PackageName where
  toField = Sql.toField <<< toString

fromCabal :: Cabal.PackageName -> PackageName
fromCabal = PackageName

fromString :: String -> Maybe PackageName
fromString = map fromCabal <<< Cabal.simpleParsec

fromText :: Text -> Maybe PackageName
fromText = Text.unpack >>> fromString

toCabal :: PackageName -> Cabal.PackageName
toCabal (PackageName cabal) = cabal

toString :: PackageName -> String
toString = Cabal.prettyShow <<< toCabal

toText :: PackageName -> Text
toText = toString >>> Text.pack
