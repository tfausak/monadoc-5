module Monadoc.Type.Service where

import qualified Test.Hspec as Hspec

data Service
  = Server
  | Worker
  deriving (Eq, Ord, Show)

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Service" $ do

  Hspec.it "needs tests" Hspec.pending
