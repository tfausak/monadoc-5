module Monadoc.Handler.Index where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Lucid as H
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Template as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle :: App.App Wai.Request Wai.Response
handle = do
  context <- Reader.ask
  maybeUser <- Common.getCookieUser

  let config = Context.config context
  loginUrl <- Common.makeLoginUrl
  pure
    . Common.htmlResponse Http.ok200 (Common.defaultHeaders config)
    . Template.makeHtmlWith config maybeUser loginUrl
    $ H.p_ "\x1f516 Better Haskell documentation."
