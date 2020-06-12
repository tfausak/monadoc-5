module Monadoc.Type.Handler
  ( Handler
  , App.run
  , App.withConnection
  )
where

import qualified Monadoc.Type.App as App
import qualified Network.Wai as Wai

-- | A server handler for an HTTP request/response.
type Handler a = App.App Wai.Request a
