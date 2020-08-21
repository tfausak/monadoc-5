module Monadoc.Type.NotFoundException where

import qualified Control.Monad.Catch as Exception
import qualified Test.Hspec as Hspec

data NotFoundException
  = NotFoundException
  deriving (Eq, Show)

instance Exception.Exception NotFoundException

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.NotFoundException" $ do

  Hspec.it "needs tests" Hspec.pending
