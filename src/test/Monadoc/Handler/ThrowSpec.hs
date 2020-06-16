module Monadoc.Handler.ThrowSpec
  ( spec
  )
where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Handler.Throw as Throw
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Network.Wai as Wai
import qualified Test

spec :: Test.Spec
spec = Test.describe "Monadoc.Handler.Throw" $ do

  Test.describe "handle" $ do

    Test.it "works" $ do
      context <- Test.makeContext
      let
        it =
          App.run context { Context.request = Wai.defaultRequest } Throw.handle
      it `Test.shouldThrow` aTestException

aTestException :: Exception.SomeException -> Bool
aTestException =
  (== Just TestException.TestException)
    . Exception.fromException
    . WithCallStack.withoutCallStack
