module Monadoc.Handler.Search where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.SQLite.Simple as Sql
import qualified Lucid as H
import Monadoc.Prelude
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Server.Template as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle :: App.App Wai.Request Wai.Response
handle = do
  context <- Reader.ask
  maybeUser <- Common.getCookieUser
  loginUrl <- Common.makeLoginUrl

  let
    config = Context.config context
    query =
      context
        |> Context.request
        |> Wai.queryString
        |> lookup "query"
        |> join
        |> maybe "" Utf8.toText

  rows <- App.sql
    "select distinct package from exported_identifiers \
    \where package like ? order by package asc limit 10"
    [query]

  let
    content = do
      H.p_ do
        "You searched for "
        H.code_ <| H.toHtml query
        "."
      rows
        |> map Sql.fromOnly
        |> map
             (\name ->
               name
                 |> PackageName.toText
                 |> H.toHtml
                 |> H.a_
                      [ H.href_
                        <| Router.renderAbsoluteRoute config
                        <| Route.Package name
                      ]
             )
        |> map H.li_
        |> fold
        |> H.ul_

  pure
    <| Common.htmlResponse Http.ok200 (Common.defaultHeaders config)
    <| Template.makeHtmlWith config maybeUser loginUrl content
