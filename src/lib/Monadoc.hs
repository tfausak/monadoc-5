module Monadoc ( monadoc ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package

monadoc :: IO ()
monadoc = do
  putStrLn $ "monadoc-" <> Version.showVersion Package.version
  Sql.withConnection ":memory:" $ \ _ -> Async.race_ server worker

server :: IO ()
server = Warp.run 8080 $ \ _ respond ->
  respond $ Wai.responseLBS Http.ok200 [] LazyByteString.empty

worker :: IO ()
worker = Monad.forever $ do
  Concurrent.threadDelay 1000000
