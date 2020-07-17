module Monadoc.Handler.ThrowSpec where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Handler.Throw as Throw
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Network.Wai as Wai
import Test

spec :: Spec
spec = describe "Monadoc.Handler.Throw" $ do

  describe "handle" $ do

    it "works" $ do
      context <- makeContext
      let
        result =
          App.run context { Context.request = Wai.defaultRequest } Throw.handle
      result `shouldThrow` aTestException

aTestException :: Exception.SomeException -> Bool
aTestException =
  (== Just TestException.TestException)
    . Exception.fromException
    . WithCallStack.withoutCallStack
