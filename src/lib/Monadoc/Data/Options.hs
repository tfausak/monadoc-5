module Monadoc.Data.Options where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import Monadoc.Prelude
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Service as Service
import qualified Network.Wai.Handler.Warp as Warp
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
  , hackageUrlOption
  , helpOption
  , hostOption
  , portOption
  , servicesOption
  , urlOption
  , versionOption
  ]

clientIdOption :: Option
clientIdOption =
  option
      []
      ["client-id"]
      ("Sets the OAuth application client ID. Defaults to "
      <> show (Config.clientId Config.initial)
      <> " which is appropriate for development."
      )
    . argument "STRING"
    $ \clientId config -> Right config { Config.clientId = clientId }

clientSecretOption :: Option
clientSecretOption =
  option
      []
      ["client-secret"]
      ("Sets the OAuth application client secret. Defaults to "
      <> show (Config.clientSecret Config.initial)
      <> "."
      )
    . argument "STRING"
    $ \clientSecret config ->
        Right config { Config.clientSecret = clientSecret }

databaseOption :: Option
databaseOption =
  option
      []
      ["database"]
      ("Sets the SQLite database file. Set this to \":memory:\" to use an \
      \in-memory database. Defaults to "
      <> show (Config.database Config.initial)
      <> "."
      )
    . argument "FILE"
    $ \database config -> Right config { Config.database = database }

discordUrlOption :: Option
discordUrlOption =
  option
      []
      ["discord-url"]
      ("Sets the Discord webhook URL. Defaults to "
      <> show (Config.discordUrl Config.initial)
      <> " which will disable exception reporting."
      )
    . argument "URL"
    $ \discordUrl config -> Right config { Config.discordUrl = discordUrl }

hackageUrlOption :: Option
hackageUrlOption =
  option
      []
      ["hackage-url"]
      ("Sets the Hackage base URL. Defaults to "
      <> show (Config.hackageUrl Config.initial)
      <> "."
      )
    . argument "URL"
    $ \hackageUrl config -> Right config { Config.hackageUrl = hackageUrl }

helpOption :: Option
helpOption =
  option ['h'] ["help"] "Shows this help message and exits."
    . GetOpt.NoArg
    $ \config -> Right config { Config.help = True }

hostOption :: Option
hostOption =
  option
      []
      ["host"]
      ("Sets the host that the server binds on. Use '*' for other machines to \
      \see your server. Defaults to "
      <> showHost (Config.host Config.initial)
      <> "."
      )
    . argument "STRING"
    $ \host config -> Right config { Config.host = String.fromString host }

showHost :: Warp.HostPreference -> String
showHost host = case host of
  "*" -> "\"*\""
  "*4" -> "\"*4\""
  "!4" -> "\"!4\""
  "*6" -> "\"*6\""
  "!6" -> "\"!6\""
  _ -> drop 5 $ show host

portOption :: Option
portOption =
  option
      []
      ["port"]
      ("Sets the port that the server binds on. Defaults to "
      <> showPort (Config.port Config.initial)
      <> "."
      )
    . argument "NUMBER"
    $ \rawPort config -> case Read.readMaybe rawPort of
        Nothing -> Left $ "invalid port: " <> show rawPort
        Just port -> Right config { Config.port = port }

showPort :: Warp.Port -> String
showPort = show . show

servicesOption :: Option
servicesOption =
  option
      []
      ["services"]
      ("Sets the services to run. Separate services with commas. Defaults to "
      <> showServices (Config.services Config.initial)
      <> " which is all the services."
      )
    . argument "STRING"
    $ \rawServices config -> case readServices rawServices of
        Nothing -> Left $ "invalid services: " <> show rawServices
        Just services -> Right config { Config.services = services }

readServices :: String -> Maybe (Set.Set Service.Service)
readServices string = do
  list <- mapM readService . Text.splitOn "," $ Text.pack string
  Monad.guard . not $ null list
  let set = Set.fromList list
  Monad.guard $ length set == length list
  pure set

readService :: Text.Text -> Maybe Service.Service
readService text = case text of
  "server" -> Just Service.Server
  "worker" -> Just Service.Worker
  _ -> Nothing

showServices :: Set.Set Service.Service -> String
showServices = show . List.intercalate "," . fmap showService . Set.toList

showService :: Service.Service -> String
showService service = case service of
  Service.Server -> "server"
  Service.Worker -> "worker"

urlOption :: Option
urlOption =
  option
      []
      ["url"]
      ("Sets the base URL that the server is available at. Defaults to "
      <> show (Config.url Config.initial)
      <> "."
      )
    . argument "URL"
    $ \url config -> Right config { Config.url = url }

versionOption :: Option
versionOption =
  option ['v'] ["version"] "Shows the version number and exits."
    . GetOpt.NoArg
    $ \config -> Right config { Config.version = True }

option
  :: String -> [String] -> String -> GetOpt.ArgDescr a -> GetOpt.OptDescr a
option c s = flip $ GetOpt.Option c s

argument :: String -> (String -> a) -> GetOpt.ArgDescr a
argument = flip GetOpt.ReqArg
