module Monadoc.Server.Main where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Console as Console
import qualified Monadoc.Server.Application as Application
import qualified Monadoc.Server.Middleware as Middleware
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Hspec as Hspec

run :: App.App request ()
run = do
  Console.info "Starting server ..."
  context <- Reader.ask
  Trans.lift
    . Warp.runSettings (Settings.fromContext context)
    . Middleware.middleware context
    $ Application.application context

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Server.Main" $ do

  Hspec.it "needs tests" Hspec.pending
