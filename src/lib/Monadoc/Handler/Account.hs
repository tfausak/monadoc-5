module Monadoc.Handler.Account
  ( handle
  )
where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Lucid as Html
import qualified Monadoc.Handler.Index as Index
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle :: App.App Wai.Request Wai.Response
handle = do
  config <- Reader.asks Context.config
  let headers = Common.defaultHeaders config
  maybeUser <- Index.getCookieUser
  loginUrl <- Index.makeLoginUrl
  pure $ case maybeUser of
    Nothing -> Common.statusResponse Http.found302
      $ Map.insert Http.hLocation (Text.encodeUtf8 loginUrl) headers
    Just _ -> Common.htmlResponse Http.ok200 headers
      . Index.makeHtmlWith config maybeUser loginUrl
      . Html.p_
      $ do
        "You are logged in. You can manage your "
        Html.a_
          [ Html.href_
          . Text.pack
          $ "https://github.com/settings/connections/applications/"
          <> Config.clientId config
          ]
          "OAuth application"
        " access on GitHub. Or you can "
        Html.form_
          [ Html.method_ "post"
          , Html.action_ $ Router.renderAbsoluteRoute config Route.LogOut
          ] $ Html.input_ [Html.type_ "submit", Html.value_ "log out"]
        "."
