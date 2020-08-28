module Monadoc.Handler.ThrowSpec where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc
import qualified Monadoc.Handler.Throw as Throw
import Monadoc.Prelude
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Network.Wai as Wai
import Test.Hspec

spec :: Spec
spec = describe "Monadoc.Handler.Throw" $ do

  describe "handle" $ do

    it "works" $ do
      ctx <- Monadoc.configToContext Config.test
      let
        result =
          App.run ctx { Context.request = Wai.defaultRequest } Throw.handle
      result `shouldThrow` aTestException

aTestException :: Exception.SomeException -> Bool
aTestException =
  (== Just TestException.TestException)
    <<< Exception.fromException
    <<< WithCallStack.withoutCallStack
