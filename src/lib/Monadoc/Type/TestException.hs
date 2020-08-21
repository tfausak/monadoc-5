module Monadoc.Type.TestException where

import qualified Control.Monad.Catch as Exception
import qualified Test.Hspec as Hspec

data TestException
  = TestException
  deriving (Eq, Show)

instance Exception.Exception TestException

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.TestException" $ do

  Hspec.it "needs tests" Hspec.pending
