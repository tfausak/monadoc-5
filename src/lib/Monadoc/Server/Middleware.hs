module Monadoc.Server.Middleware
  ( middleware
  )
where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified GHC.Clock as Clock
import qualified Monadoc.Console as Console
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified System.Mem as Mem
import qualified Text.Printf as Printf

middleware :: Config.Config -> Wai.Middleware
middleware config = logRequests . handleExceptions config . handleEtag

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
    then Common.responseBS
      Http.notModified304
      (Map.fromList $ Wai.responseHeaders response)
      ByteString.empty
    else response
