module Monadoc.Worker.Main
  ( run
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Binary as Binary
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Type.Path as Path
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Size as Size
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified System.FilePath as FilePath
import qualified System.IO.Unsafe as Unsafe

run :: App.App request ()
run = do
  Console.info "Starting worker ..."
  Monad.forever $ do
    Console.info "Worker running ..."
    pruneBlobs
    updateIndex
    processIndex
    Console.info "Worker waiting ..."
    sleep $ 15 * 60

pruneBlobs :: App.App request ()
pruneBlobs = App.withConnection $ \connection -> Trans.lift $ do
  rows <- Sql.query_
    connection
    "select blobs.sha256 \
    \from blobs \
    \left join files \
    \on files.digest = blobs.sha256 \
    \where files.digest is null"
  let count = length rows
  Monad.when (count > 0) $ do
    Console.info $ unwords ["Pruning", pluralize "orphan blob" count, "..."]
    mapM_
      (Sql.execute connection "delete from blobs where sha256 = ?")
      (rows :: [Sql.Only Sha256.Sha256])

pluralize :: String -> Int -> String
pluralize word count =
  unwords [show count, if count == 1 then word else word <> "s"]

updateIndex :: App.App request ()
updateIndex = do
  etag <- getEtag
  Console.info $ unwords ["Updating Hackage index with", show etag, "..."]
  request <- buildRequest etag
  response <- getResponse request
  case Http.statusCode $ Client.responseStatus response of
    200 -> handle200 response
    304 -> handle304
    _ -> handleOther request response

indexUrl :: String
indexUrl = "https://hackage.haskell.org/01-index.tar.gz"

getEtag :: App.App request Etag.Etag
getEtag = do
  rows <- App.withConnection $ \connection -> Trans.lift
    $ Sql.query connection "select etag from cache where url = ?" [indexUrl]
  pure $ case rows of
    [] -> Etag.fromByteString ByteString.empty
    Sql.Only etag : _ -> etag

buildRequest :: Etag.Etag -> App.App request Client.Request
buildRequest etag = do
  initialRequest <- Client.parseRequest indexUrl
  pure $ addRequestHeader
    Http.hIfNoneMatch
    (Etag.toByteString etag)
    initialRequest

getResponse
  :: Client.Request
  -> App.App request (Client.Response LazyByteString.ByteString)
getResponse request = do
  manager <- Reader.asks Context.manager
  Trans.lift $ Client.httpLbs request manager

handle200 :: Client.Response LazyByteString.ByteString -> App.App request ()
handle200 response = do
  let
    etag =
      Etag.fromByteString
        . Maybe.fromMaybe ByteString.empty
        . lookup Http.hETag
        $ Client.responseHeaders response
  Console.info $ mconcat ["Hackage index has changed to ", show etag, "."]
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
      "insert into cache (etag, sha256, url) values (?, 'unused', ?) \
      \ on conflict (url) do update set \
      \ etag = excluded.etag, sha256 = excluded.sha256"
      (etag, indexUrl)
    Sql.execute
      connection
      "insert into files (digest, name) values (?, ?) \
      \ on conflict (name) do update set \
      \ digest = excluded.digest"
      (sha256, indexPath)

indexPath :: Path.Path
indexPath = Path.fromFilePath "hackage/01-index.tar.gz"

handle304 :: App.App request ()
handle304 = Console.info "Hackage index has not changed."

handleOther
  :: Client.Request
  -> Client.Response LazyByteString.ByteString
  -> App.App request ()
handleOther request response =
  WithCallStack.throw
    . Client.HttpExceptionRequest request
    . Client.StatusCodeException (response { Client.responseBody = () })
    . LazyByteString.toStrict
    $ Client.responseBody response

processIndex :: App.App request ()
processIndex = do
  maybeSha256 <- getSha256
  case maybeSha256 of
    Nothing -> do
      Console.info $ mconcat ["Missing SHA256 for ", show indexUrl, "."]
      removeCache
    Just sha256 -> do
      maybeBinary <- getBinary sha256
      case maybeBinary of
        Nothing -> do
          Console.info $ mconcat ["Missing binary for ", show sha256, "."]
          removeCache
        Just binary -> processIndexWith binary

getSha256 :: App.App request (Maybe Sha256.Sha256)
getSha256 = do
  rows <- App.withConnection $ \connection -> Trans.lift $ Sql.query
    connection
    "select digest from files where name = ?"
    [indexPath]
  pure $ case rows of
    [] -> Nothing
    Sql.Only sha256 : _ -> Just sha256

removeCache :: App.App request ()
removeCache = App.withConnection $ \connection -> Trans.lift
  $ Sql.execute connection "delete from cache where url = ?" [indexUrl]

getBinary :: Sha256.Sha256 -> App.App request (Maybe Binary.Binary)
getBinary sha256 = do
  rows <- App.withConnection $ \connection -> Trans.lift $ Sql.query
    connection
    "select octets from blobs where sha256 = ?"
    [sha256]
  pure $ case rows of
    [] -> Nothing
    Sql.Only binary : _ -> Just binary

processIndexWith :: Binary.Binary -> App.App request ()
processIndexWith binary = do
  var <- Trans.lift $ Stm.newTVarIO Map.empty
  mapM_ (processEntry var)
    . Tar.foldEntries (:) [] unsafeThrow
    . Tar.read
    . Gzip.decompress
    . LazyByteString.fromStrict
    $ Binary.toByteString binary

processEntry
  :: Stm.TVar (Map.Map Cabal.PackageName (Map.Map Cabal.Version Word))
  -> Tar.Entry
  -> App.App request ()
processEntry var entry = case Tar.entryContent entry of
  Tar.NormalFile lazyContents _size ->
    let
      path = Path.fromFilePath $ Tar.entryPath entry
      strictContents = LazyByteString.toStrict lazyContents
      digest = Sha256.fromDigest $ Crypto.hash strictContents
    in case FilePath.takeExtension $ Tar.entryPath entry of
      "" -> pure () -- TODO: preferred-versions
      ".cabal" -> do
        -- Get the package name and version from the path.
        (packageName, version) <- case Path.toStrings path of
          [rawPackageName, rawVersion, _] -> do
            packageName <- case Cabal.simpleParsec rawPackageName of
              Nothing ->
                WithCallStack.throw
                  . userError
                  $ "invalid package name: "
                  <> show rawPackageName
              Just packageName -> pure packageName
            version <- case Cabal.simpleParsec rawVersion of
              Nothing ->
                WithCallStack.throw
                  . userError
                  $ "invalid version: "
                  <> show rawVersion
              Just version -> pure version
            pure (packageName, version)
          strings ->
            WithCallStack.throw
              . userError
              $ "unexpected path: "
              <> show strings
        -- Get the revision number and update the map.
        revision <- Trans.lift . Stm.atomically $ do
          allRevisions <- Stm.readTVar var
          case Map.lookup packageName allRevisions of
            Nothing -> do
              let revision = 0
              Stm.modifyTVar var . Map.insert packageName $ Map.singleton
                version
                revision
              pure revision
            Just packageRevisions ->
              case Map.lookup version packageRevisions of
                Nothing -> do
                  let revision = 0
                  Stm.modifyTVar var
                    $ Map.adjust (Map.insert version revision) packageName
                  pure revision
                Just versionRevision -> do
                  let revision = versionRevision + 1
                  Stm.modifyTVar var
                    $ Map.adjust (Map.insert version revision) packageName
                  pure revision
        -- Build the new path with revision.
        let
          newPath = Path.fromStrings
            [ Cabal.prettyShow packageName
            , Cabal.prettyShow version
            , show revision
            , Cabal.prettyShow packageName <> ".cabal"
            ]
        -- Upsert the package description.
        App.withConnection $ \connection -> Trans.lift $ do
          rows <- Sql.query
            connection
            "select digest from files where name = ?"
            [newPath]
          case rows of
            [] -> do
              Sql.execute
                connection
                "insert into blobs (octets, sha256, size) values (?, ?, ?) \
                  \on conflict (sha256) do nothing"
                ( Binary.fromByteString strictContents
                , digest
                , Size.fromInt $ ByteString.length strictContents
                )
              Sql.execute
                connection
                "insert into files (digest, name) values (?, ?) \
                  \on conflict (name) do update set digest = excluded.digest"
                (digest, newPath)
            Sql.Only expected : _ ->
              Monad.when (digest /= expected)
                . WithCallStack.throw
                . userError
                $ show (expected, digest, entry)
      ".json" -> pure () -- ignore
      _ -> WithCallStack.throw $ UnknownExtension entry
  _ -> WithCallStack.throw $ UnknownEntry entry

newtype UnknownExtension
  = UnknownExtension Tar.Entry
  deriving (Eq, Show)

instance Exception.Exception UnknownExtension

newtype UnknownEntry
  = UnknownEntry Tar.Entry
  deriving (Eq, Show)

instance Exception.Exception UnknownEntry

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
