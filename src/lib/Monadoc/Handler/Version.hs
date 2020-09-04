module Monadoc.Handler.Version where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Database.SQLite.Simple as Sql
import qualified Lucid as H
import Monadoc.Prelude
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Template as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Monadoc.Type.Cabal.Version as Version
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Revision as Revision
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle
  :: PackageName.PackageName
  -> Version.Version
  -> App.App Wai.Request Wai.Response
handle name version = do
  context <- Reader.ask
  maybeUser <- Common.getCookieUser
  loginUrl <- Common.makeLoginUrl
  rows <- App.sql
    "select distinct revision from exported_identifiers where package = ? and version = ?"
    (name, version)

  let
    config = Context.config context
    headers = Common.defaultHeaders config
    status = if blank rows then Http.notFound404 else Http.ok200
    content = if blank rows
      then H.p_ do
        "Could not find a package named "
        H.code_ <| H.toHtml <| PackageName.toText name
        " with version number "
        H.code_ <| H.toHtml <| Version.toText version
        "."
      else
        rows
        |> map Sql.fromOnly
        |> List.sortOn Ord.Down
        |> map (Revision.toString >>> H.toHtml >>> H.li_)
        |> fold
        |> H.ul_

  pure
    <| Common.htmlResponse status headers
    <| Template.makeHtmlWith config maybeUser loginUrl content
