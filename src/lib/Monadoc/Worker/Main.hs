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
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified GHC.Stack as Stack
import qualified Monadoc.Console as Console
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Binary as Binary
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Monadoc.Type.Cabal.VersionRange as VersionRange
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Type.Path as Path
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Size as Size
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Monadoc.Vendor.Sql as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified System.FilePath as FilePath
import qualified System.IO.Unsafe as Unsafe

run :: Stack.HasCallStack => App.App request ()
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
pruneBlobs = do
  rows <- App.sql
    "select blobs.sha256 \
    \from blobs \
    \left join files \
    \on files.digest = blobs.sha256 \
    \where files.digest is null"
    ()
  let count = length rows
  Monad.when (count > 0) $ do
    Console.info $ unwords ["Pruning", pluralize "orphan blob" count, "..."]
    mapM_
      (App.sql_ "delete from blobs where sha256 = ?")
      (rows :: [Sql.Only Sha256.Sha256])

pluralize :: String -> Int -> String
pluralize word count =
  unwords [show count, if count == 1 then word else word <> "s"]

updateIndex :: Stack.HasCallStack => App.App request ()
updateIndex = do
  etag <- getEtag
  Console.info $ unwords ["Updating Hackage index with", show etag, "..."]
  request <- buildRequest etag
  response <- getResponse request
  case Http.statusCode $ Client.responseStatus response of
    200 -> handle200 response
    304 -> handle304
    _ -> handleOther request response

getIndexUrl :: App.App request String
getIndexUrl = do
  hackageUrl <- Reader.asks $ Config.hackageUrl . Context.config
  pure $ hackageUrl <> "/01-index.tar.gz"

getEtag :: App.App request Etag.Etag
getEtag = do
  indexUrl <- getIndexUrl
  rows <- App.sql "select etag from cache where url = ?" [indexUrl]
  pure $ case rows of
    [] -> Etag.fromByteString ByteString.empty
    Sql.Only etag : _ -> etag

buildRequest :: Etag.Etag -> App.App request Client.Request
buildRequest etag = do
  indexUrl <- getIndexUrl
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
  let
    body = LazyByteString.toStrict $ Client.responseBody response
    sha256 = Sha256.fromDigest $ Crypto.hash body
  upsertBlob body sha256
  indexUrl <- getIndexUrl
  App.sql_
    "insert into cache (etag, sha256, url) values (?, 'unused', ?) \
    \ on conflict (url) do update set \
    \ etag = excluded.etag, sha256 = excluded.sha256"
    (etag, indexUrl)
  App.sql_
    "insert into files (digest, name) values (?, ?) \
    \ on conflict (name) do update set \
    \ digest = excluded.digest"
    (sha256, indexPath)

indexPath :: Path.Path
indexPath = Path.fromFilePath "hackage/01-index.tar.gz"

handle304 :: App.App request ()
handle304 = Console.info "Hackage index has not changed."

handleOther
  :: Stack.HasCallStack
  => Client.Request
  -> Client.Response LazyByteString.ByteString
  -> App.App request ()
handleOther request response =
  WithCallStack.throw
    . Client.HttpExceptionRequest request
    . Client.StatusCodeException (response { Client.responseBody = () })
    . LazyByteString.toStrict
    $ Client.responseBody response

processIndex :: Stack.HasCallStack => App.App request ()
processIndex = do
  maybeSha256 <- getSha256
  case maybeSha256 of
    Nothing -> do
      indexUrl <- getIndexUrl
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
  rows <- App.sql "select digest from files where name = ?" [indexPath]
  pure $ case rows of
    [] -> Nothing
    Sql.Only sha256 : _ -> Just sha256

removeCache :: App.App request ()
removeCache = do
  indexUrl <- getIndexUrl
  App.sql_ "delete from cache where url = ?" [indexUrl]

getBinary :: Sha256.Sha256 -> App.App request (Maybe Binary.Binary)
getBinary sha256 = do
  rows <- App.sql "select octets from blobs where sha256 = ?" [sha256]
  pure $ case rows of
    [] -> Nothing
    Sql.Only binary : _ -> Just binary

processIndexWith :: Stack.HasCallStack => Binary.Binary -> App.App request ()
processIndexWith binary = do
  countVar <- Trans.lift $ Stm.newTVarIO 1
  revisionsVar <- Trans.lift $ Stm.newTVarIO Map.empty
  versionsVar <- Trans.lift $ Stm.newTVarIO Map.empty
  mapM_ (processEntry countVar revisionsVar versionsVar)
    . Tar.foldEntries (:) [] unsafeThrow
    . Tar.read
    . Gzip.decompress
    . LazyByteString.fromStrict
    $ Binary.toByteString binary
  versions <- Trans.lift . Stm.atomically $ Stm.readTVar versionsVar
  Console.info $ unwords
    ["Updating", pluralize "preferred version" $ Map.size versions, "..."]
  Monad.forM_ (Map.toList versions) $ \(packageName, versionRange) -> App.sql_
    "insert into preferred_versions (package_name, version_range) \
    \values (?, ?) \
    \on conflict (package_name) \
    \do update set version_range = excluded.version_range"
    (packageName, versionRange)

processEntry
  :: Stm.TVar Word
  -> Stm.TVar (Map.Map PackageName.PackageName (Map.Map Cabal.Version Word))
  -> Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange)
  -> Tar.Entry
  -> App.App request ()
processEntry countVar revisionsVar versionsVar entry = do
  count <- Trans.lift . Stm.atomically $ do
    count <- Stm.readTVar countVar
    Stm.modifyTVar countVar (+ 1)
    pure count
  Monad.when (rem count 1000 == 0)
    . Console.info
    . unwords
    $ ["Processing entry number", show count, "..."]
  case Tar.entryContent entry of
    Tar.NormalFile lazyContents _size ->
      let
        path = Path.fromFilePath $ Tar.entryPath entry
        strictContents = LazyByteString.toStrict lazyContents
        digest = Sha256.fromDigest $ Crypto.hash strictContents
      in case FilePath.takeExtension $ Tar.entryPath entry of
        "" -> processPreferredVersion versionsVar path strictContents
        ".cabal" ->
          processPackageDescription revisionsVar path strictContents digest
        ".json" -> processPackageSignature path strictContents digest
        _ -> WithCallStack.throw $ UnknownExtension entry
    _ -> WithCallStack.throw $ UnknownEntry entry

processPreferredVersion
  :: Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange)
  -> Path.Path
  -> ByteString.ByteString
  -> App.App request ()
processPreferredVersion versionsVar path strictContents = do
  packageName <- case Path.toStrings path of
    [rawPackageName, "preferred-versions"] ->
      case PackageName.fromString rawPackageName of
        Nothing -> WithCallStack.throw $ InvalidPackageName rawPackageName
        Just packageName -> pure packageName
    strings -> WithCallStack.throw $ UnexpectedPath strings
  versionRange <- case Utf8.toString strictContents of
    "" -> pure $ VersionRange.fromCabal Cabal.anyVersion
    string -> case Cabal.simpleParsec string of
      Nothing -> WithCallStack.throw $ InvalidVersionConstraint string
      Just (Cabal.PackageVersionConstraint pn vr) ->
        if pn == PackageName.toCabal packageName
          then pure $ VersionRange.fromCabal vr
          else WithCallStack.throw $ PackageNameMismatch packageName pn
  Trans.lift . Stm.atomically . Stm.modifyTVar versionsVar $ Map.insert
    packageName
    versionRange

processPackageDescription
  :: Stm.TVar (Map.Map PackageName.PackageName (Map.Map Cabal.Version Word))
  -> Path.Path
  -> ByteString.ByteString
  -> Sha256.Sha256
  -> App.App request ()
processPackageDescription revisionsVar path strictContents digest = do
  -- Get the package name and version from the path.
  (packageName, version) <- case Path.toStrings path of
    [rawPackageName, rawVersion, _] -> do
      packageName <- case PackageName.fromString rawPackageName of
        Nothing -> WithCallStack.throw $ InvalidPackageName rawPackageName
        Just packageName -> pure packageName
      version <- case Cabal.simpleParsec rawVersion of
        Nothing -> WithCallStack.throw $ InvalidVersionNumber rawVersion
        Just version -> pure version
      pure (packageName, version)
    strings -> WithCallStack.throw $ UnexpectedPath strings
  -- Get the revision number and update the map.
  revision <- Trans.lift . Stm.atomically $ do
    allRevisions <- Stm.readTVar revisionsVar
    case Map.lookup packageName allRevisions of
      Nothing -> do
        let revision = 0
        Stm.modifyTVar revisionsVar . Map.insert packageName $ Map.singleton
          version
          revision
        pure revision
      Just packageRevisions -> case Map.lookup version packageRevisions of
        Nothing -> do
          let revision = 0
          Stm.modifyTVar revisionsVar
            $ Map.adjust (Map.insert version revision) packageName
          pure revision
        Just versionRevision -> do
          let revision = versionRevision + 1
          Stm.modifyTVar revisionsVar
            $ Map.adjust (Map.insert version revision) packageName
          pure revision
  -- Build the new path with revision.
  let
    newPath = Path.fromStrings
      [ PackageName.toString packageName
      , Cabal.prettyShow version
      , show revision
      , PackageName.toString packageName <> ".cabal"
      ]
  -- Upsert the package description.
  rows <- App.sql "select digest from files where name = ?" [newPath]
  case rows of
    [] -> pure ()
    Sql.Only expected : _ ->
      Monad.when (digest /= expected) . Console.warn $ mconcat
        [ "Digest of "
        , show newPath
        , " changed from "
        , show expected
        , " to "
        , show digest
        , "!"
        ]
  upsertBlob strictContents digest
  App.sql_
    "insert into files (digest, name) values (?, ?) \
    \on conflict (name) do update set digest = excluded.digest"
    (digest, newPath)

-- For now we are essentially ignoring these entries. In the future we may want
-- to do something with them. That's why we're storing them in the database.
-- The entries contain a JSON object that describes the expected hash of the
-- package tarball. For example:
--
-- { "signatures": []
-- , "signed":
--   { "_type": "Targets"
--   , "expires": null
--   , "targets":
--     { "<repo>/package/transformers-compose-0.1.tar.gz":
--       { "hashes":
--         { "md5": "3fab..."
--         , "sha256": "cddc..."
--         }
--       , "length": 3328
--       }
--     }
--   , "version": 0
--   }
-- }
processPackageSignature
  :: Path.Path
  -> ByteString.ByteString
  -> Sha256.Sha256
  -> App.App request ()
processPackageSignature path strictContents digest = do
  (packageName, version) <- case Path.toStrings path of
    [rawPackageName, rawVersion, "package.json"] -> do
      packageName <- case PackageName.fromString rawPackageName of
        Nothing -> WithCallStack.throw $ InvalidPackageName rawPackageName
        Just packageName -> pure packageName
      version <- case Cabal.simpleParsec rawVersion of
        Nothing -> WithCallStack.throw $ InvalidVersionNumber rawVersion
        Just version -> pure version
      pure (packageName, version)
    strings -> WithCallStack.throw $ UnexpectedPath strings
  let
    newPath = Path.fromStrings
      [ PackageName.toString packageName
      , Cabal.prettyShow (version :: Cabal.Version)
      , "package.json"
      ]
  rows <- App.sql "select digest from files where name = ?" [newPath]
  case rows of
    [] -> pure ()
    Sql.Only expected : _ ->
      Monad.when (digest /= expected) . Console.warn $ mconcat
        [ "Digest of "
        , show newPath
        , " changed from "
        , show expected
        , " to "
        , show digest
        , "!"
        ]
  upsertBlob strictContents digest
  App.sql_
    "insert into files (digest, name) values (?, ?) \
    \on conflict (name) do update set digest = excluded.digest"
    (digest, newPath)

newtype InvalidPackageName
  = InvalidPackageName String
  deriving Show

instance Exception.Exception InvalidPackageName

newtype InvalidVersionNumber
  = InvalidVersionNumber String
  deriving Show

instance Exception.Exception InvalidVersionNumber

newtype UnexpectedPath
  = UnexpectedPath [String]
  deriving Show

instance Exception.Exception UnexpectedPath

newtype InvalidVersionConstraint
  = InvalidVersionConstraint String
  deriving Show

instance Exception.Exception InvalidVersionConstraint

data PackageNameMismatch
  = PackageNameMismatch PackageName.PackageName Cabal.PackageName
  deriving Show

instance Exception.Exception PackageNameMismatch

upsertBlob :: ByteString.ByteString -> Sha256.Sha256 -> App.App request ()
upsertBlob contents sha256 = do
  rows <- App.sql "select count(*) from blobs where sha256 = ?" [sha256]
  let count = maybe (0 :: Int) Sql.fromOnly $ Maybe.listToMaybe rows
  Monad.when (count < 1) $ App.sql_
    "insert into blobs (octets, sha256, size) values (?, ?, ?)"
    ( Binary.fromByteString contents
    , sha256
    , Size.fromInt $ ByteString.length contents
    )

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
