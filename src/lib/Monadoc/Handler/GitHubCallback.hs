module Monadoc.Handler.GitHubCallback
  ( handle
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
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

handle :: Stack.HasCallStack => App.App Wai.Request Wai.Response
handle = do
  maybeCode <- getCode
  code <- case maybeCode of
    Nothing -> WithCallStack.throw $ userError "no code"
    Just code -> pure code

  maybeToken <- getToken code
  token <- case maybeToken of
    Nothing -> WithCallStack.throw $ userError "no token"
    Just token -> pure token

  maybeUser <- getUser token
  user <- case maybeUser of
    Nothing -> WithCallStack.throw $ userError "no user"
    Just user -> pure user

  maybeGuid <- upsertUser token user
  guid <- case maybeGuid of
    Nothing -> WithCallStack.throw $ userError "no guid"
    Just guid -> pure guid

  cookie <- Common.makeCookie guid
  redirect <- getRedirect
  config <- Reader.asks Context.config
  pure
    . Common.statusResponse Http.found302
    . Map.insert Http.hLocation redirect
    . Map.insert Http.hSetCookie (Common.renderCookie cookie)
    $ Common.defaultHeaders config

getCode :: App.App Wai.Request (Maybe ByteString.ByteString)
getCode =
  Reader.asks $ Monad.join . lookup "code" . Wai.queryString . Context.request

getToken :: ByteString.ByteString -> App.App request (Maybe Text.Text)
getToken code = do
  context <- Reader.ask
  initialRequest <- Client.parseUrlThrow
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
  pure
    . Monad.join
    . lookup "access_token"
    . Http.parseQueryText
    . LazyByteString.toStrict
    $ Client.responseBody response

getUser :: Text.Text -> App.App request (Maybe GHUser.User)
getUser token = do
  context <- Reader.ask
  initialRequest <- Client.parseUrlThrow "https://api.github.com/user"
  let
    request = initialRequest
      { Client.requestHeaders =
        [ (Http.hAuthorization, "Bearer " <> Utf8.fromText token)
        , (Http.hUserAgent, Settings.serverName)
        ]
      }
  response <- Trans.lift . Client.httpLbs request $ Context.manager context
  pure . Aeson.decode $ Client.responseBody response

upsertUser :: Text.Text -> GHUser.User -> App.App request (Maybe Guid.Guid)
upsertUser token ghUser = do
  guid <- Trans.lift $ Random.getStdRandom Guid.random
  let
    user = User.User
      { User.guid = guid
      , User.id = GHUser.id ghUser
      , User.login = GHUser.login ghUser
      , User.token = token
      }
  App.withConnection $ \connection ->
    Trans.lift . Sql.withTransaction connection $ do
      Sql.execute
        connection
        "insert into users (guid, id, login, token) values (?, ?, ?, ?) \
        \on conflict (id) do update set \
        \login = excluded.login, token = excluded.token"
        user
      rows <- Sql.query
        connection
        "select guid from users where id = ?"
        [User.id user]
      pure . fmap Sql.fromOnly $ Maybe.listToMaybe rows

getRedirect :: App.App Wai.Request ByteString.ByteString
getRedirect =
  Reader.asks
    $ Maybe.fromMaybe "/"
    . Monad.join
    . lookup "redirect"
    . Wai.queryString
    . Context.request
