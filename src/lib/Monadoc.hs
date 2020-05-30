module Monadoc ( monadoc ) where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as Http

monadoc :: IO ()
monadoc = Warp.run 8080 $ \ _ respond ->
  respond $ Wai.responseLBS Http.ok200 [] LazyByteString.empty
