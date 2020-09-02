module Monadoc.Server.Main where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import Monadoc.Prelude
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Utility.Console as Console
import qualified Network.Wai.Handler.Warp as Warp

run :: App.App request ()
run = do
  Console.info "Starting server ..."
  context <- Reader.ask
  Trans.lift
    <<< Warp.runSettings (Settings.fromContext context)
    <<< Middleware.middleware context
    <| Application.application context
