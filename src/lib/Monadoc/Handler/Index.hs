module Monadoc.Handler.Index where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Lucid as H
import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Server.Template as Template
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Test.Hspec as Hspec

handle :: App.App Wai.Request Wai.Response
handle = do
  context <- Reader.ask
  maybeUser <- Common.getCookieUser

  let config = Context.config context
  loginUrl <- Common.makeLoginUrl
  pure
    . Common.htmlResponse Http.ok200 (Common.defaultHeaders config)
    . Template.makeHtmlWith config maybeUser loginUrl
    $ H.p_ "\x1f516 Better Haskell documentation."

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Handler.Index" $ do

  Hspec.describe "handle" $ do

    Hspec.it "works" $ do
      Hspec.pending
      -- ctx <- makeContext
      -- response <- App.run
      --   ctx { Context.request = Wai.defaultRequest }
      --   handle
      -- Wai.responseStatus response `Hspec.shouldBe` Http.ok200
