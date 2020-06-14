module Monadoc.Handler.GitHubCallback
  ( handle
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.GitHub.UserId as UserId
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.Random as Random

handle :: Stack.HasCallStack => App.App Wai.Request result
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
  request1 <- do
    initial <- Client.parseUrlThrow
      "https://github.com/login/oauth/access_token"
    let config = Context.config context
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
  user <-
    either (WithCallStack.throw . userError) pure
    . Aeson.eitherDecode
    $ Client.responseBody response2

  -- TODO: Store user information.
  guid <- Trans.lift $ Random.getStdRandom Guid.random
  Console.info $ show (guid, user :: User, token)

  -- TODO: Redirect with cookie.
  WithCallStack.throw $ userError "TODO"

data User = User
  { userId :: UserId.UserId
  , userLogin :: Text.Text
  } deriving (Eq, Show)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" $ \object -> do
    id_ <- object Aeson..: "id"
    login <- object Aeson..: "login"
    pure User { userId = id_, userLogin = login }
