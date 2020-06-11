module Monadoc.Server.Middleware
  ( middleware
  )
where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified GHC.Clock as Clock
import qualified Monadoc.Console as Console
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified System.Mem as Mem
import qualified Text.Printf as Printf

middleware :: Wai.Middleware
middleware = logRequests . addContentLength . handleExceptions

logRequests :: Wai.Middleware
logRequests handle request respond = do
  timeBefore <- Clock.getMonotonicTime
  allocationsBefore <- Mem.getAllocationCounter
  handle request $ \response -> do
    allocationsAfter <- Mem.getAllocationCounter
    timeAfter <- Clock.getMonotonicTime
    Console.info $ Printf.printf
      "%d %s %s%s %.3f %d"
      (Http.statusCode $ Wai.responseStatus response)
      (Utf8.toString $ Wai.requestMethod request)
      (Utf8.toString $ Wai.rawPathInfo request)
      (Utf8.toString $ Wai.rawQueryString request)
      (timeAfter - timeBefore)
      (div (allocationsBefore - allocationsAfter) 1024)
    respond response

addContentLength :: Wai.Middleware
addContentLength handle request respond = handle request $ \oldResponse ->
  respond $ case oldResponse of
    Wai.ResponseBuilder status headers builder ->
      let
        size = LazyByteString.length $ Builder.toLazyByteString builder
        header = (Http.hContentLength, Utf8.fromString $ show size)
      in Wai.ResponseBuilder status (header : headers) builder
    _ -> oldResponse

handleExceptions :: Wai.Middleware
handleExceptions handle request respond =
  Exception.catch (handle request respond) $ \someException -> do
    Settings.onException (Just request) someException
    respond $ Settings.onExceptionResponse someException
