module Monadoc.Type.App
  ( App
  , run
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified GHC.Stack as Stack
import qualified Monadoc.Type.Context as Context

-- | The main application type. This simply provides the run-time context. Use
-- 'run' to convert this into 'IO'.
type App a = Reader.ReaderT Context.Context IO a

-- | Runs an 'App' action. Be sure to decorate everything with
-- 'Stack.HasCallStack' constraints in order to get call stacks everywhere.
run :: Stack.HasCallStack => Context.Context -> App a -> IO a
run = flip Reader.runReaderT
