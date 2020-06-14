module Monadoc.Handler.GitHubCallback
  ( handle
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as Uuid
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
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
  context <- Reader.ask

  -- Get information from request.
  Console.info . show $ Context.request context
  body <- Trans.lift . Wai.lazyRequestBody $ Context.request context
  Console.info $ show body
  code <- case lookup "code" . Wai.queryString $ Context.request context of
    Just (Just x) -> pure x
    _ -> WithCallStack.throw $ userError "no code"

  -- Validate code with GitHub.
  let config = Context.config context
  request1 <- do
    initial <- Client.parseUrlThrow
      "https://github.com/login/oauth/access_token"
    pure $ Client.urlEncodedBody
      [ ("client_id", Utf8.fromString $ Config.clientId config)
      , ("client_secret", Utf8.fromString $ Config.clientSecret config)
      , ("code", code)
      ]
      initial
  let manager = Context.manager context
  response1 <- Trans.lift $ Client.httpLbs request1 manager
  Console.info $ show response1
  token <-
    case
      lookup "access_token"
      . Http.parseQueryText
      . LazyByteString.toStrict
      $ Client.responseBody response1
    of
      Just (Just x) -> pure x
      _ -> WithCallStack.throw $ userError "no access token"

  -- Get user information.
  request2 <- do
    initial <- Client.parseUrlThrow "https://api.github.com/user"
    pure initial
      { Client.requestHeaders =
        [ (Http.hAuthorization, "Bearer " <> Text.encodeUtf8 token)
        , (Http.hUserAgent, Settings.serverName)
        ]
      }
  response2 <- Trans.lift $ Client.httpLbs request2 manager
  Console.info $ show response2
  ghUser <-
    either (WithCallStack.throw . userError) pure
    . Aeson.eitherDecode
    $ Client.responseBody response2

  -- Store user information.
  newGuid <- Trans.lift $ Random.getStdRandom Guid.random
  let
    user = User.User
      { User.guid = newGuid
      , User.id = GHUser.id ghUser
      , User.login = GHUser.login ghUser
      , User.token = token
      }
  guid <- App.withConnection $ \connection ->
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
      case rows of
        [] -> WithCallStack.throw $ userError "no user"
        row : _ -> pure $ Sql.fromOnly row
  Console.info $ show user

  pure
    . Common.statusResponse Http.found302
    . Map.insert Http.hLocation "/"
    . Map.insert
        Http.hSetCookie
        (mconcat
          [ "guid="
          , Uuid.toASCIIBytes $ Guid.toUuid guid
          , "; HttpOnly; Path=/; SameSite=Strict"
          , if Common.isSecure config then "; Secure" else ""
          ]
        )
    $ Common.defaultHeaders config
