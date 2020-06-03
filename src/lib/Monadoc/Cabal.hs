module Monadoc.Cabal
  ( Errors(..)
  , Package(..)
  , parse
  )
where

import qualified Data.ByteString
import qualified Data.Function
import qualified Data.List.NonEmpty
import qualified Distribution.PackageDescription.Parsec
import qualified Distribution.Parsec.Error
import qualified Distribution.Types.GenericPackageDescription

newtype Errors = Errors
  { unwrapErrors :: Data.List.NonEmpty.NonEmpty Distribution.Parsec.Error.PError
  } deriving Show

instance Eq Errors where
  (==) = Data.Function.on (==) show

newtype Package = Package
  { unwrapPackage :: Distribution.Types.GenericPackageDescription.GenericPackageDescription
  } deriving (Eq, Show)

parse :: Data.ByteString.ByteString -> Either Errors Package
parse byteString =
  let
    parseResult =
      Distribution.PackageDescription.Parsec.parseGenericPackageDescription
        byteString
  in
    case
      snd $ Distribution.PackageDescription.Parsec.runParseResult parseResult
    of
      Left (_, x) -> Left $ Errors x
      Right x -> Right $ Package x
