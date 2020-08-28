module Monadoc.Utility.Cabal where

import qualified Data.ByteString as ByteString
import qualified Data.Function as Function
import qualified Data.List.NonEmpty as NonEmpty
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec.Error as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import Monadoc.Prelude

newtype Errors = Errors
  { unwrapErrors :: NonEmpty.NonEmpty Cabal.PError
  } deriving Show

instance Eq Errors where
  (==) = Function.on (==) show

newtype Package = Package
  { unwrapPackage :: Cabal.GenericPackageDescription
  } deriving (Eq, Show)

parse :: ByteString.ByteString -> Either Errors Package
parse byteString =
  let parseResult = Cabal.parseGenericPackageDescription byteString
  in
    case snd $ Cabal.runParseResult parseResult of
      Left (_, x) -> Left $ Errors x
      Right x -> Right $ Package x
