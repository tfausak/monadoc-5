module Monadoc.Handler.Account
  ( handle
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
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
  case maybeUser of
    Nothing -> do
      loginUrl <- Index.makeLoginUrl
      pure
        . Common.statusResponse Http.found302
        $ Map.insert Http.hLocation (Text.encodeUtf8 loginUrl) headers
    Just _ -> pure $ Common.statusResponse Http.ok200 headers
