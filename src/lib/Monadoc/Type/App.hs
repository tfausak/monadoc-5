module Monadoc.Type.App
  ( App
  , run
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified GHC.Stack as Stack
import qualified Monadoc.Type.Context as Context

-- | The main application type. This simply provides the run-time context and
-- enforces a call stack constraint. (Due to the constraint, you'll probably
-- need rank-N types.)
type App a = Reader.ReaderT Context.Context IO a

run :: Stack.HasCallStack => Context.Context -> App a -> IO a
run = flip Reader.runReaderT
