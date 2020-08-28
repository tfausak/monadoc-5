module Monadoc.Data.Version where

import qualified Data.Version as Version
import qualified Paths_monadoc as Package
import Monadoc.Prelude

-- | The canonical string representation of the 'version'.
string :: String
string = Version.showVersion version

-- | This package's version number. Usually you won't need this at run time,
-- but it can be useful for error messages or diagnostics.
version :: Version.Version
version = Package.version
