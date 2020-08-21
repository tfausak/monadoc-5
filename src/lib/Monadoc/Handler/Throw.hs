module Monadoc.Handler.Throw where

import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.TestException as TestException
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Test.Hspec as Hspec

handle :: App.App request result
handle = WithCallStack.throw TestException.TestException

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Handler.Throw" $ do

  Hspec.describe "handle" $ do

    Hspec.it "works" $ do
      Hspec.pending
--       ctx <- makeContext
--       let
--         result =
--           App.run ctx { Context.request = Wai.defaultRequest } handle
--       result `Hspec.shouldThrow` aTestException

-- aTestException :: Exception.SomeException -> Bool
-- aTestException =
--   (== Just TestException.TestException)
--     . Exception.fromException
--     . WithCallStack.withoutCallStack
