module Monadoc.Handler.Account
  ( handle
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Handler.Index as Index
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle :: App.App Wai.Request Wai.Response
handle = do
  config <- Reader.asks Context.config
  let headers = Common.defaultHeaders config
  maybeUser <- Index.getCookieUser
  pure $ case maybeUser of
    Nothing -> Common.statusResponse Http.forbidden403 headers
    Just _ -> Common.statusResponse Http.ok200 headers
