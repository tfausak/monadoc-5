module Monadoc.Data.Commit
  ( hash
  )
where

-- | The Git commit hash this package was built at, if available. Like the
-- version number, you'll probably only need this for diagnostics.
hash :: Maybe String
hash = Nothing
