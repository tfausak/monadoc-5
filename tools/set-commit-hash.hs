module Main ( main ) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Environment as Environment

main :: IO ()
main = do
  [ file, hash ] <- Environment.getArgs
  contents <- Text.readFile file
  Text.writeFile file $ Text.replace
    (Text.pack "Nothing")
    (Text.pack $ mconcat ["(Just ", show hash, ")"])
    contents
