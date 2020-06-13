module Monadoc.Handler.Tachyons
  ( handle
  )
where

import qualified Monadoc.Server.Common as Common
import qualified Monadoc.Type.App as App
import qualified Network.Wai as Wai

handle :: App.App request Wai.Response
handle =
  Common.simpleFileResponse "tachyons-4-12-0.css" "text/css;charset=utf-8"