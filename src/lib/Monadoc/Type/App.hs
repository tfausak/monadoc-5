module Monadoc.Type.App
  ( App
  , run
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Type.Context as Context

-- | The main application type. This simply provides the run-time context. Use
-- 'run' to convert this into 'IO'.
type App a = Reader.ReaderT Context.Context IO a

-- | Runs an 'App' action.
run :: Context.Context -> App a -> IO a
run = flip Reader.runReaderT
