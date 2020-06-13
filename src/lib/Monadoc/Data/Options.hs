module Monadoc.Data.Options
  ( options
  )
where

import qualified Data.String as String
import qualified Monadoc.Type.Config as Config
import qualified System.Console.GetOpt as GetOpt
import qualified Text.Read as Read

type Option = GetOpt.OptDescr (Config.Config -> Either String Config.Config)

-- | Collection of command-line options. Run the app with @--help@ to see what
-- they are.
options :: [Option]
options =
  [ clientIdOption
  , clientSecretOption
  , databaseOption
  , discordUrlOption
  , helpOption
  , hostOption
  , portOption
  , urlOption
  , versionOption
  ]

clientIdOption :: Option
clientIdOption =
  option [] ["client-id"] "sets the client ID (defaults to 235ce...)"
    . argument "STRING"
    $ \clientId config -> Right config { Config.clientId = clientId }

clientSecretOption :: Option
clientSecretOption =
  option [] ["client-secret"] "sets the client secret (defaults to 48e20...)"
    . argument "STRING"
    $ \clientSecret config ->
        Right config { Config.clientSecret = clientSecret }

databaseOption :: Option
databaseOption =
  option [] ["database"] "sets the database file (defaults to monadoc.sqlite3)"
    . argument "FILE"
    $ \database config -> Right config { Config.database = database }

discordUrlOption :: Option
discordUrlOption =
  option
      []
      ["discord-url"]
      "sets the Discord webhook URL (defaults to http://discord.invalid/...)"
    . argument "URL"
    $ \discordUrl config -> Right config { Config.discordUrl = discordUrl }

helpOption :: Option
helpOption =
  option ['h'] ["help"] "shows the help and exits" . GetOpt.NoArg $ \config ->
    Right config { Config.help = True }

hostOption :: Option
hostOption =
  option [] ["host"] "sets the server host (defaults to 127.0.0.1)"
    . argument "STRING"
    $ \host config -> Right config { Config.host = String.fromString host }

portOption :: Option
portOption =
  option [] ["port"] "sets the server port (defaults to 4444)"
    . argument "NUMBER"
    $ \rawPort config -> case Read.readMaybe rawPort of
        Nothing -> Left $ "invalid port: " <> show rawPort
        Just port -> Right config { Config.port = port }

urlOption :: Option
urlOption =
  option [] ["url"] "sets the base URL (defaults to http://localhost:4444)"
    . argument "URL"
    $ \url config -> Right config { Config.url = url }

versionOption :: Option
versionOption =
  option ['v'] ["version"] "shows the version number and exits"
    . GetOpt.NoArg
    $ \config -> Right config { Config.version = True }

option
  :: String -> [String] -> String -> GetOpt.ArgDescr a -> GetOpt.OptDescr a
option c s = flip $ GetOpt.Option c s

argument :: String -> (String -> a) -> GetOpt.ArgDescr a
argument = flip GetOpt.ReqArg
