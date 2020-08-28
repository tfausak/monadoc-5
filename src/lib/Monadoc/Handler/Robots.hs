module Monadoc.Handler.Robots where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Monadoc.Prelude

handle :: App.App request Wai.Response
handle = do
  config <- Reader.asks Context.config
  pure
    . Common.stringResponse Http.ok200 (Common.defaultHeaders config)
    $ unlines ["User-agent: *", "Disallow:"]
