module Monadoc.Server.Middleware
  ( middleware
  )
where

import qualified Codec.Compression.GZip as Gzip
import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified GHC.Clock as Clock
import qualified Monadoc.Console as Console
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified System.Mem as Mem
import qualified Text.Printf as Printf

middleware :: Config.Config -> Wai.Middleware
middleware config =
  logRequests . handleExceptions config . handleEtag . compress

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

handleExceptions :: Config.Config -> Wai.Middleware
handleExceptions config handle request respond =
  Exception.catch (handle request respond) $ \someException -> do
    Settings.onException (Just request) someException
    respond $ Settings.onExceptionResponse config someException

handleEtag :: Wai.Middleware
handleEtag handle request respond = handle request $ \response ->
  let
    isGet = Wai.requestMethod request == Http.methodGet
    isSuccessful = Http.statusIsSuccessful $ Wai.responseStatus response
    expected = lookup Http.hIfNoneMatch $ Wai.requestHeaders request
    hasEtag = Maybe.isJust expected
    actual = lookup Http.hETag $ Wai.responseHeaders response
  in respond $ if isGet && isSuccessful && hasEtag && actual == expected
    then Wai.responseLBS
      Http.notModified304
      (filter (\header -> not $ isContentLength header || isETag header)
      $ Wai.responseHeaders response
      )
      LazyByteString.empty
    else response

isContentLength :: Http.Header -> Bool
isContentLength = (== Http.hContentLength) . fst

isETag :: Http.Header -> Bool
isETag = (== Http.hETag) . fst

compress :: Wai.Middleware
compress handle request respond = handle request $ \response ->
  respond $ case response of
    Wai.ResponseBuilder status headers builder ->
      let
        expanded = Builder.toLazyByteString builder
        compressed = Gzip.compress expanded
        contentLength =
          Utf8.fromString . show $ LazyByteString.length compressed
        newHeaders =
          (Http.hContentEncoding, "gzip")
            : (Http.hContentLength, contentLength)
            : filter (not . isContentLength) headers
      in if acceptsGzip request
           && not (isEncoded response)
           && longEnough expanded
         then
           Wai.responseLBS status newHeaders compressed
         else
           response
    _ -> response

isEncoded :: Wai.Response -> Bool
isEncoded = Maybe.isJust . lookup Http.hContentEncoding . Wai.responseHeaders

acceptsGzip :: Wai.Request -> Bool
acceptsGzip =
  elem "gzip"
    . fmap Text.strip
    . Text.splitOn ","
    . Text.decodeUtf8With Text.lenientDecode
    . Maybe.fromMaybe ""
    . lookup Http.hAcceptEncoding
    . Wai.requestHeaders

longEnough :: LazyByteString.ByteString -> Bool
longEnough = (> 1024) . LazyByteString.length
