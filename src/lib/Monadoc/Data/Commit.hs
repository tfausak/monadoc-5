{-# LANGUAGE TemplateHaskell #-}

module Monadoc.Data.Commit
  ( hash
  )
where

import qualified GitHash

gitInfo :: Either String GitHash.GitInfo
gitInfo = $$( GitHash.tGitInfoCwdTry )

-- | The Git commit hash this package was built at, if available. Like the
-- version number, you'll probably only need this for diagnostics.
hash :: Maybe String
hash = either (const Nothing) (Just . GitHash.giHash) gitInfo
