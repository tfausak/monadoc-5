module Monadoc.Server.Main
  ( run
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
import qualified Monadoc.Data.Commit as Commit
import qualified Monadoc.Data.Version as Version
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

run :: Stack.HasCallStack => App.App ()
run = do
  context <- Reader.ask
  Trans.lift
    . Warp.runSettings (makeSettings $ Context.config context)
    $ \request respond -> App.run context $ do
        let method = fromUtf8 $ Wai.requestMethod request
        let path = Text.unpack <$> Wai.pathInfo request
        case (method, path) of
          ("GET", ["health-check"]) ->
            Trans.lift . respond $ statusResponse Http.ok200 []
          ("GET", ["throw"]) -> WithCallStack.throw $ userError "oh no"
          _ -> Trans.lift . respond $ statusResponse Http.notFound404 []

makeSettings :: Config.Config -> Warp.Settings
makeSettings config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Config.host config)
    . Warp.setLogger logger
    . Warp.setOnException onException
    . Warp.setOnExceptionResponse onExceptionResponse
    . Warp.setPort (Config.port config)
    $ Warp.setServerName serverName Warp.defaultSettings

beforeMainLoop :: Config.Config -> IO ()
beforeMainLoop config = Console.info $ unwords
  ["Listening on", show $ Config.host config, "port", show $ Config.port config]

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = Console.info $ unwords
  [ show $ Http.statusCode status
  , fromUtf8 $ Wai.requestMethod request
  , fromUtf8 $ Wai.rawPathInfo request <> Wai.rawQueryString request
  ]

onException :: Maybe Wai.Request -> Exception.SomeException -> IO ()
onException _ someException@(Exception.SomeException exception) =
  Monad.when (Warp.defaultShouldDisplayException someException)
    . Console.warn
    $ Exception.displayException exception

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = statusResponse Http.internalServerError500 []

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = stringResponse status headers
  $ unwords
      [show $ Http.statusCode status, fromUtf8 $ Http.statusMessage status]

stringResponse :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
stringResponse status headers string = Wai.responseLBS
  status
  ((Http.hContentType, toUtf8 "text/plain; charset=utf-8") : headers)
  (LazyByteString.fromStrict $ toUtf8 string)

fromUtf8 :: ByteString.ByteString -> String
fromUtf8 = Text.unpack . Text.decodeUtf8With Text.lenientDecode

toUtf8 :: String -> ByteString.ByteString
toUtf8 = Text.encodeUtf8 . Text.pack

serverName :: ByteString.ByteString
serverName = toUtf8 $ "monadoc-" <> Version.string <> case Commit.hash of
  Nothing -> ""
  Just hash -> "-" <> hash
