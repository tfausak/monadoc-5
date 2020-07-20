module Monadoc.Handler.GitHubCallback where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.GitHub.User as GHUser
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.User as User
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified System.Random as Random

handle :: App.App Wai.Request Wai.Response
handle = do
  code <- getCode
  token <- getToken code
  user <- getUser token
  guid <- upsertUser token user

  cookie <- Common.makeCookie guid
  redirect <- getRedirect
  config <- Reader.asks Context.config
  pure
    . Common.statusResponse Http.found302
    . Map.insert Http.hLocation redirect
    . Map.insert Http.hSetCookie (Common.renderCookie cookie)
    $ Common.defaultHeaders config

getCode :: App.App Wai.Request ByteString.ByteString
getCode = do
  request <- Reader.asks Context.request
  case lookup "code" $ Wai.queryString request of
    Just (Just code) -> pure code
    _ -> WithCallStack.throw $ NoCodeProvided request

newtype NoCodeProvided
  = NoCodeProvided Wai.Request
  deriving Show

instance Exception.Exception NoCodeProvided

getToken :: ByteString.ByteString -> App.App request Text.Text
getToken code = do
  context <- Reader.ask
  initialRequest <- Client.parseRequest
    "https://github.com/login/oauth/access_token"
  let
    config = Context.config context
    request = Client.urlEncodedBody
      [ ("client_id", Utf8.fromString $ Config.clientId config)
      , ("client_secret", Utf8.fromString $ Config.clientSecret config)
      , ("code", code)
      ]
      initialRequest
  response <- Trans.lift . Client.httpLbs request $ Context.manager context
  case
      lookup "access_token"
      . Http.parseQueryText
      . LazyByteString.toStrict
      $ Client.responseBody response
    of
      Just (Just token) -> pure token
      _ -> WithCallStack.throw $ TokenRequestFailed request response

data TokenRequestFailed
  = TokenRequestFailed Client.Request (Client.Response LazyByteString.ByteString)
  deriving Show

instance Exception.Exception TokenRequestFailed

getUser :: Text.Text -> App.App request GHUser.User
getUser token = do
  context <- Reader.ask
  initialRequest <- Client.parseRequest "https://api.github.com/user"
  let
    request = initialRequest
      { Client.requestHeaders =
        [ (Http.hAuthorization, "Bearer " <> Utf8.fromText token)
        , (Http.hUserAgent, Settings.serverName)
        ]
      }
  response <- Trans.lift . Client.httpLbs request $ Context.manager context
  case Aeson.eitherDecode $ Client.responseBody response of
    Right user -> pure user
    Left message ->
      WithCallStack.throw $ UserRequestFailed request response message

data UserRequestFailed
  = UserRequestFailed Client.Request (Client.Response LazyByteString.ByteString) String
  deriving Show

instance Exception.Exception UserRequestFailed

upsertUser :: Text.Text -> GHUser.User -> App.App request Guid.Guid
upsertUser token ghUser = do
  guid <- Trans.lift $ Random.getStdRandom Guid.random
  let
    user = User.User
      { User.guid = guid
      , User.id = GHUser.id ghUser
      , User.login = GHUser.login ghUser
      , User.token = token
      }
  App.sql_
    "insert into users (guid, id, login, token) values (?, ?, ?, ?) \
    \on conflict (id) do update set \
    \login = excluded.login, token = excluded.token"
    user
  rows <- App.sql "select guid from users where id = ?" [User.id user]
  case rows of
    only : _ -> pure $ Sql.fromOnly only
    _ -> WithCallStack.throw $ UserUpsertFailed user

newtype UserUpsertFailed
  = UserUpsertFailed User.User
  deriving Show

instance Exception.Exception UserUpsertFailed

getRedirect :: App.App Wai.Request ByteString.ByteString
getRedirect =
  Reader.asks
    $ Maybe.fromMaybe "/"
    . Monad.join
    . lookup "redirect"
    . Wai.queryString
    . Context.request
