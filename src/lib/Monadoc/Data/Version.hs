module Monadoc.Data.Version where

import qualified Data.Version as Version
import qualified Paths_monadoc as Package
import qualified Test.Hspec as Hspec

-- | The canonical string representation of the 'version'.
string :: String
string = Version.showVersion version

-- | This package's version number. Usually you won't need this at run time,
-- but it can be useful for error messages or diagnostics.
version :: Version.Version
version = Package.version

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Data.Version" $ do

  Hspec.describe "string" $ do

    Hspec.it "is not null" $ do
      string `Hspec.shouldSatisfy` not . null

  Hspec.describe "version" $ do

    Hspec.it "has four branches" $ do
      Version.versionBranch version `Hspec.shouldSatisfy` (== 4) . length

    Hspec.it "has no tags" $ do
      let Version.Version _ tags = version
      tags `Hspec.shouldSatisfy` null
