module Monadoc.Handler.Account where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Lucid as H
import Monadoc.Prelude
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Server.Template as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle :: App.App Wai.Request Wai.Response
handle = do
  config <- Reader.asks Context.config
  let headers = Common.defaultHeaders config
  maybeUser <- Common.getCookieUser
  loginUrl <- Common.makeLoginUrl
  pure <| case maybeUser of
    Nothing ->
      Common.statusResponse Http.found302
        <| Map.insert Http.hLocation (Utf8.fromText loginUrl) headers
    Just _ ->
      Common.htmlResponse Http.ok200 headers
        <<< Template.makeHtmlWith config maybeUser loginUrl
        <<< H.form_
              [ H.method_ "post"
              , H.action_ <| Router.renderAbsoluteRoute config Route.LogOut
              ]
        <<< H.p_
        <| do
             "You are logged in. You can manage your "
             H.a_
               [ H.href_
                 <<< Text.pack
                 <| "https://github.com/settings/connections/applications/"
                 <> Config.clientId config
               ]
               "OAuth application"
             " access on GitHub. Or you can "
             H.input_
               [ H.type_ "submit"
               , H.value_ "log out"
               , H.class_ "bg-inherit bn input-reset pa0 pointer red underline"
               ]
             " of Monadoc."
