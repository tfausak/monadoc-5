module Monadoc.Server.Main
  ( run
  )
where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified GHC.Stack as Stack
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Network.Wai.Handler.Warp as Warp

run :: Stack.HasCallStack => App.App request ()
run = do
  context <- Reader.ask
  Trans.lift
    . Warp.runSettings (Settings.fromContext context)
    . Middleware.middleware context
    $ Application.application context
