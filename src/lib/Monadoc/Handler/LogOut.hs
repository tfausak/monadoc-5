module Monadoc.Handler.LogOut
  ( handle
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as Uuid
import qualified Monadoc.Handler.GitHubCallback as GitHubCallback
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Vendor.Time as Time
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

handle :: App.App Wai.Request Wai.Response
handle = do
  config <- Reader.asks Context.config
  cookie <- GitHubCallback.makeCookie $ Guid.fromUuid Uuid.nil
  let
    headers = Map.union (Common.defaultHeaders config) $ Map.fromList
      [ ( Http.hLocation
        , Text.encodeUtf8 $ Router.renderAbsoluteRoute config Route.Index
        )
      , ( Http.hSetCookie
        , Common.renderCookie cookie
          { Cookie.setCookieExpires = Just $ Time.utcTime 2000 1 1 0 0 0
          }
        )
      ]
  pure $ Common.statusResponse Http.found302 headers
