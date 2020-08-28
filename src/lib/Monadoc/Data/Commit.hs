module Monadoc.Data.Commit where

import Monadoc.Prelude

-- | The Git commit hash this package was built at, if available. Like the
-- version number, you'll probably only need this for diagnostics.
hash :: Maybe String
hash =
  -- This looks pretty silly by itself. See src/script/set-commit-hash.hs for
  -- an explanation.
  Nothing
