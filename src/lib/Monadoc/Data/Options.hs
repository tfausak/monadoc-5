module Monadoc.Data.Options
  ( options
  )
where

import qualified Data.String as String
import qualified Monadoc.Type.Config as Config
import qualified System.Console.GetOpt as GetOpt
import qualified Text.Read as Read

options :: [GetOpt.OptDescr (Config.Config -> Either String Config.Config)]
options =
  [ option
      []
      ["database"]
      "sets the database file (defaults to monadoc.sqlite3)"
    . argument "FILE"
    $ \database config -> Right config { Config.database = database }
  , option ['h'] ["help"] "shows the help and exits"
    . GetOpt.NoArg
    $ \config -> Right config { Config.help = True }
  , option [] ["host"] "sets the server host (defaults to 127.0.0.1)"
    . argument "STRING"
    $ \host config -> Right config { Config.host = String.fromString host }
  , option [] ["port"] "sets the server port (defaults to 4444)"
    . argument "NUMBER"
    $ \rawPort config -> case Read.readMaybe rawPort of
        Nothing -> Left $ "invalid port: " <> show rawPort
        Just port -> Right config { Config.port = port }
  , option ['v'] ["version"] "shows the version number and exits"
    . GetOpt.NoArg
    $ \config -> Right config { Config.version = True }
  ]

option
  :: [Char] -> [String] -> String -> GetOpt.ArgDescr a -> GetOpt.OptDescr a
option c s = flip $ GetOpt.Option c s

argument :: String -> (String -> a) -> GetOpt.ArgDescr a
argument = flip GetOpt.ReqArg
