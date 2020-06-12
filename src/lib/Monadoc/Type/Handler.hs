module Monadoc.Type.Handler
  ( Handler
  , run
  , withConnection
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Pool as Pool
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.Wai as Wai

-- | A server handler for an HTTP request/response.
type Handler a = Reader.ReaderT (Context.Context Wai.Request, Wai.Request) IO a

-- | Given a context and a request, runs the handler to produce the response.
-- Note that this could throw an exception, so be sure to handle those
-- somewhere else.
run :: Context.Context () -> Wai.Request -> Handler a -> IO a
run context request =
  flip Reader.runReaderT (context { Context.request = request }, request)

-- | Checks out a SQL connection from the pool and runs the given action with
-- it.
withConnection :: (Sql.Connection -> Handler a) -> Handler a
withConnection action = do
  pool <- Reader.asks $ Context.pool . fst
  Pool.withResource pool action
