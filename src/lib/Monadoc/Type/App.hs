{-# LANGUAGE RankNTypes #-}

module Monadoc.Type.App
  ( App
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified GHC.Stack as Stack
import qualified Monadoc.Type.Context as Context

-- | The main application type. This simply provides the run-time context and
-- enforces a call stack constraint. (Due to the constraint, you'll probably
-- need rank-N types.)
type App a = Stack.HasCallStack => Reader.ReaderT Context.Context IO a
