module Monadoc.Server.Main
  ( run
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

run :: Stack.HasCallStack => App.App ()
run = do
  context <- Reader.ask
  Trans.lift
    . Warp.runSettings (Settings.fromConfig $ Context.config context)
    $ \request respond -> App.run context $ do
        let method = Utf8.toString $ Wai.requestMethod request
        let path = Text.unpack <$> Wai.pathInfo request
        case (method, path) of
          ("GET", ["health-check"]) ->
            Trans.lift . respond $ statusResponse Http.ok200 []
          ("GET", ["throw"]) -> WithCallStack.throw $ userError "oh no"
          _ -> Trans.lift . respond $ statusResponse Http.notFound404 []

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = stringResponse status headers $ unwords
  [show $ Http.statusCode status, Utf8.toString $ Http.statusMessage status]

stringResponse :: Http.Status -> Http.ResponseHeaders -> String -> Wai.Response
stringResponse status headers string = Wai.responseLBS
  status
  ((Http.hContentType, Utf8.fromString "text/plain; charset=utf-8") : headers)
  (LazyByteString.fromStrict $ Utf8.fromString string)
