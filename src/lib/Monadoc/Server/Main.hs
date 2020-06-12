module Monadoc.Server.Main
  ( run
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.Wai.Handler.Warp as Warp

run :: App.App () ()
run = do
  context <- Reader.ask
  let config = Context.config context
  Trans.lift
    . Warp.runSettings (Settings.fromConfig config)
    . Middleware.middleware config
    $ Application.application context
