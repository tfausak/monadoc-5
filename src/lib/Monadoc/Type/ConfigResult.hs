module Monadoc.Type.ConfigResult where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Monadoc.Type.Config as Config
import qualified Test.Hspec as Hspec

-- | The result of attempting to get the config from the environment.
data ConfigResult
  = Failure (NonEmpty.NonEmpty String)
  -- ^ Getting the config failed with one or more errors. Each error will start
  -- with @"ERROR: "@ and end with a newline.
  | ExitWith String
  -- ^ Getting the config succeeded, but the program should exit early with the
  -- given message. This is used when showing the help or version number. The
  -- message will end with a newline.
  | Success [String] Config.Config
  -- ^ Getting the config succeeded in spite of some warnings. Each warning
  -- will start with @"WARNING: "@ and end with a newline.
  deriving (Eq, Show)

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.ConfigResult" $ do

  Hspec.it "needs tests" Hspec.pending
