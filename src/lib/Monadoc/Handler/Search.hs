module Monadoc.Handler.Search where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Lucid as H
import Monadoc.Prelude
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Template as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle :: App.App Wai.Request Wai.Response
handle = do
  context <- Reader.ask
  maybeUser <- Common.getCookieUser
  let
    query =
      case lookup "query" <<< Wai.queryString <| Context.request context of
        Just (Just byteString) -> Utf8.toText byteString
        _ -> ""

  let config = Context.config context
  loginUrl <- Common.makeLoginUrl
  pure
    <<< Common.htmlResponse Http.ok200 (Common.defaultHeaders config)
    <<< Template.makeHtmlWith config maybeUser loginUrl
    <<< H.p_
    <| do
         "Your query was "
         H.code_ <| H.toHtml query
         " but search is not implemented yet."
