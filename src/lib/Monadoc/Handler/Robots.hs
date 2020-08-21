module Monadoc.Handler.Robots where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test.Hspec as Hspec

handle :: App.App request Wai.Response
handle = do
  config <- Reader.asks Context.config
  pure
    . Common.stringResponse Http.ok200 (Common.defaultHeaders config)
    $ unlines ["User-agent: *", "Disallow:"]

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Handler.Robots" $ do

  Hspec.describe "handle" $ do

    Hspec.it "works" $ do
      Hspec.pending
      -- ctx <- makeContext
      -- response <- App.run
      --   ctx { Context.request = Wai.defaultRequest }
      --   handle
      -- Wai.responseStatus response `Hspec.shouldBe` Http.ok200
