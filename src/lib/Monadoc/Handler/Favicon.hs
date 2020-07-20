module Monadoc.Handler.Favicon where

import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Network.Wai as Wai

handle :: App.App request Wai.Response
handle = Common.simpleFileResponse "favicon.ico" "image/x-icon"
