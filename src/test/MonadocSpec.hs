module MonadocSpec ( spec ) where

import qualified Monadoc
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.it "" $ Monadoc.unit `Hspec.shouldBe` ()
