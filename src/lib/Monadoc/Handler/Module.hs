module Monadoc.Handler.Module where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.List as List
import qualified Database.SQLite.Simple as Sql
import qualified Lucid as H
import Monadoc.Prelude
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Router as Router
import qualified Monadoc.Server.Template as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Cabal.ModuleName as ModuleName
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Monadoc.Type.Cabal.Version as Version
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Route as Route
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle
  :: PackageName.PackageName
  -> Version.Version
  -> Revision.Revision
  -> ModuleName.ModuleName
  -> App.App Wai.Request Wai.Response
handle pkg ver rev mod = do
  context <- Reader.ask
  maybeUser <- Common.getCookieUser
  loginUrl <- Common.makeLoginUrl
  rows <- App.sql @(Sql.Only Text)
    "select identifier from exported_identifiers \
    \where package = ? and version = ? and revision = ? and module = ?"
    (pkg, ver, rev, mod)

  let
    config = Context.config context
    headers = Common.defaultHeaders config
    status = if blank rows then Http.notFound404 else Http.ok200
    content = if blank rows
      then H.p_ do
        "Could not find a module named "
        H.code_ <| H.toHtml <| ModuleName.toText mod
        " in the package named "
        H.code_ <| H.toHtml <| PackageName.toText pkg
        " with version number "
        H.code_ <| H.toHtml <| Version.toText ver
        " and revision number "
        H.code_ <| H.toHtml <| Revision.toText rev
        "."
      else
        rows
        |> map Sql.fromOnly
        |> List.sort
        |> map
             (\idn ->
               idn
                 |> H.toHtml
                 |> H.code_
                 |> H.a_
                      [ H.href_
                        <| Router.renderAbsoluteRoute config
                        <| Route.Identifier pkg ver rev mod idn
                      ]
                 |> H.li_
             )
        |> fold
        |> H.ul_

  pure
    <| Common.htmlResponse status headers
    <| Template.makeHtmlWith config maybeUser loginUrl content
