module Monadoc.Type.Handler
  ( Handler
  , run
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified GHC.Stack as Stack
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai as Wai

-- | A server handler for an HTTP request/response.
type Handler = Reader.ReaderT (Context.Context, Wai.Request) IO Wai.Response

-- | Given a context and a request, runs the handler to produce the response.
-- Note that this could throw an exception, so be sure to handle those
-- somewhere else.
run
  :: Stack.HasCallStack
  => Context.Context
  -> Wai.Request
  -> Handler
  -> IO Wai.Response
run context request = flip Reader.runReaderT (context, request)
