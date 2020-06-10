module Monadoc.Worker.Main
  ( run
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Binary as Binary
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Size as Size
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified System.FilePath as FilePath
import qualified System.IO.Unsafe as Unsafe

run :: App.App ()
run = Monad.forever $ do
  pruneBlobs
  etag <- updateIndex
  processIndex etag
  sleep $ 15 * 60

pruneBlobs :: App.App ()
pruneBlobs = App.withConnection $ \connection -> Trans.lift $ do
  rows <- Sql.query_
    connection
    "select blobs.sha256 \
    \from blobs \
    \left join cache \
    \on cache.sha256 = blobs.sha256 \
    \where cache.sha256 is null"
  let count = length rows
  Monad.when (count > 0) $ do
    Console.info $ unwords ["Pruning", pluralize "orphan blob" count, "..."]
    Sql.execute
      connection
      "delete from blobs where sha256 in (?)"
      (fmap Sql.fromOnly rows :: [Sha256.Sha256])

pluralize :: String -> Int -> String
pluralize word count =
  unwords [show count, if count == 1 then word else word <> "s"]

updateIndex :: App.App Etag.Etag
updateIndex = do
  etag <- getEtag
  Console.info $ unwords ["Updating Hackage index with", show etag, "..."]
  request <- buildRequest etag
  response <- getResponse request
  case Http.statusCode $ Client.responseStatus response of
    200 -> handle200 response
    304 -> handle304 etag
    _ -> handleOther request response

indexUrl :: String
indexUrl = "https://hackage.haskell.org/01-index.tar.gz"

getEtag :: App.App Etag.Etag
getEtag = do
  rows <- App.withConnection $ \connection -> Trans.lift
    $ Sql.query connection "select etag from cache where url = ?" [indexUrl]
  pure $ case rows of
    [] -> Etag.fromByteString ByteString.empty
    Sql.Only etag : _ -> etag

buildRequest :: Etag.Etag -> App.App Client.Request
buildRequest etag = do
  initialRequest <- Client.parseRequest indexUrl
  pure $ addRequestHeader
    Http.hIfNoneMatch
    (Etag.toByteString etag)
    initialRequest

getResponse
  :: Client.Request -> App.App (Client.Response LazyByteString.ByteString)
getResponse request = do
  manager <- Reader.asks Context.manager
  Trans.lift $ Client.httpLbs request manager

handle200 :: Client.Response LazyByteString.ByteString -> App.App Etag.Etag
handle200 response = do
  let
    etag =
      Etag.fromByteString
        . Maybe.fromMaybe ByteString.empty
        . lookup Http.hETag
        $ Client.responseHeaders response
  Console.info $ unwords ["Hackage index has changed to", show etag, "."]
  App.withConnection $ \connection -> Trans.lift $ do
    let
      body = LazyByteString.toStrict $ Client.responseBody response
      sha256 = Sha256.fromDigest $ Crypto.hash body
    Sql.execute
      connection
      "insert into blobs (octets, sha256, size) \
      \ values (?, ?, ?) on conflict (sha256) do nothing"
      ( Binary.fromByteString body
      , sha256
      , Size.fromInt $ ByteString.length body
      )
    Sql.execute
      connection
      "insert into cache (etag, sha256, url) values (?, ?, ?) \
      \ on conflict (url) do update set \
      \ etag = excluded.etag, sha256 = excluded.sha256"
      (etag, sha256, indexUrl)
  pure etag

handle304 :: Etag.Etag -> App.App Etag.Etag
handle304 etag = do
  Console.info "Hackage index has not changed."
  pure etag

handleOther
  :: Client.Request
  -> Client.Response LazyByteString.ByteString
  -> App.App Etag.Etag
handleOther request response =
  WithCallStack.throw
    . Client.HttpExceptionRequest request
    . Client.StatusCodeException (response { Client.responseBody = () })
    . LazyByteString.toStrict
    $ Client.responseBody response

processIndex :: Etag.Etag -> App.App ()
processIndex etag = do
  maybeSha256 <- getSha256 etag
  case maybeSha256 of
    Nothing -> do
      Console.info $ unwords ["Missing SHA256 for", show etag, "."]
      removeCache etag
    Just sha256 -> do
      maybeBinary <- getBinary sha256
      case maybeBinary of
        Nothing -> do
          Console.info $ unwords ["Missing binary for", show sha256, "."]
          removeCache etag
        Just binary -> processIndexWith binary

getSha256 :: Etag.Etag -> App.App (Maybe Sha256.Sha256)
getSha256 etag = do
  rows <- App.withConnection $ \connection -> Trans.lift
    $ Sql.query connection "select sha256 from cache where etag = ?" [etag]
  pure $ case rows of
    [] -> Nothing
    Sql.Only sha256 : _ -> Just sha256

removeCache :: Etag.Etag -> App.App ()
removeCache etag = App.withConnection $ \connection ->
  Trans.lift $ Sql.execute connection "delete from cache where etag = ?" [etag]

getBinary :: Sha256.Sha256 -> App.App (Maybe Binary.Binary)
getBinary sha256 = do
  rows <- App.withConnection $ \connection -> Trans.lift $ Sql.query
    connection
    "select octets from blobs where sha256 = ?"
    [sha256]
  pure $ case rows of
    [] -> Nothing
    Sql.Only binary : _ -> Just binary

processIndexWith :: Binary.Binary -> App.App ()
processIndexWith =
  Console.info
    . mappend "Index entry count: "
    . show
    . length
    . Tar.foldEntries
        (\entry entries -> case Tar.entryContent entry of
          Tar.NormalFile _contents _size ->
            case FilePath.takeExtension $ Tar.entryPath entry of
              "" -> entry : entries -- preferred-versions
              ".cabal" -> entry : entries
              ".json" -> entries -- ignore
              _ -> unsafeThrow . userError $ show entry
          _ -> unsafeThrow . userError $ show entry
        )
        []
        unsafeThrow
    . Tar.read
    . Gzip.decompress
    . LazyByteString.fromStrict
    . Binary.toByteString

unsafeThrow :: (Stack.HasCallStack, Exception.Exception e) => e -> a
unsafeThrow = Unsafe.unsafePerformIO . WithCallStack.throw

sleep :: IO.MonadIO m => Double -> m ()
sleep = IO.liftIO . Concurrent.threadDelay . round . (* 1000000)

addRequestHeader
  :: Http.HeaderName
  -> ByteString.ByteString
  -> Client.Request
  -> Client.Request
addRequestHeader name value request = request
  { Client.requestHeaders = (name, value) : Client.requestHeaders request
  }
