module Monadoc.Handler.Ping where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handle :: App.App request Wai.Response
handle = do
  rows <- App.sql "select 1" ()
  Monad.guard $ rows == [Sql.Only (1 :: Int)]
  config <- Reader.asks Context.config
  pure . Common.statusResponse Http.ok200 $ Common.defaultHeaders config
