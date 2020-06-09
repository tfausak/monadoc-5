{-# LANGUAGE CPP #-}

module Monadoc.Data.Commit
  ( hash
  )
where

-- | The Git commit hash this package was built at, if available. Like the
-- version number, you'll probably only need this for diagnostics.
hash :: Maybe String

#ifdef MONADOC_COMMIT_HASH

#define stringize(x) #x
hash = Just stringize(MONADOC_COMMIT_HASH)

#else

hash = Nothing

#endif
