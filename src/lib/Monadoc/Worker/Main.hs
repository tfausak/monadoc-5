module Monadoc.Worker.Main where

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
import qualified Data.Either as Either
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Data.Word as Word
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Compiler as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.System as Cabal
import qualified Distribution.Types.BuildInfo as Cabal
import qualified Distribution.Types.ComponentRequestedSpec as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.Flag as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.Library as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified GHC.Clock as Clock
import qualified GHC.LanguageExtensions.Type as G
import qualified Language.Haskell.Extension as C
import qualified Monadoc.Console as Console
import qualified Monadoc.Ghc
import qualified Monadoc.Server.Settings as Settings
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Binary as Binary
import qualified Monadoc.Type.Cabal.ModuleName as ModuleName
import qualified Monadoc.Type.Cabal.PackageName as PackageName
import qualified Monadoc.Type.Cabal.Version as Version
import qualified Monadoc.Type.Cabal.VersionRange as VersionRange
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Etag as Etag
import qualified Monadoc.Type.Path as Path
import qualified Monadoc.Type.Revision as Revision
import qualified Monadoc.Type.Sha256 as Sha256
import qualified Monadoc.Type.Size as Size
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Monadoc.Type.WithCallStack as WithCallStack
import qualified Monadoc.Utility.Cabal
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified System.FilePath as FilePath
import qualified System.IO.Unsafe as Unsafe
import qualified System.Mem as Mem
import qualified Text.Printf as Printf

run :: App.App request ()
run = do
  Console.info "Starting worker ..."
  Exception.handle sendExceptionToDiscord . Monad.forever $ do
    withLogging "worker-loop" $ do
      withLogging "prune-blobs" pruneBlobs
      withLogging "update-index" updateIndex
      withLogging "process-index" processIndex
      withLogging "fetch-tarballs" fetchTarballs
      withLogging "process-tarballs" processTarballs
      withLogging "parse-package-descriptions" parsePackageDescriptions
    sleep $ 15 * 60

withLogging :: String -> App.App request a -> App.App request a
withLogging label action = do
  Console.info $ "Starting " <> label <> " ..."
  ((result, bytes), nanoseconds) <- withNanoseconds $ withBytes action
  Console.info $ Printf.printf
    "Finished %s after %.3f seconds using %s bytes."
    label
    (fromIntegral nanoseconds / 1000000000 :: Double)
    (withCommas bytes)
  pure result

withCommas :: Show a => a -> String
withCommas = reverse . List.intercalate "," . chunksOf 3 . reverse . show

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = case splitAt n xs of
  ([], _) -> []
  (ys, zs) -> ys : chunksOf n zs

withBytes :: IO.MonadIO m => m a -> m (a, Int.Int64)
withBytes action = do
  before <- IO.liftIO Mem.getAllocationCounter
  result <- action
  after <- IO.liftIO Mem.getAllocationCounter
  pure (result, before - after)

withNanoseconds :: IO.MonadIO m => m a -> m (a, Word.Word64)
withNanoseconds action = do
  before <- IO.liftIO Clock.getMonotonicTimeNSec
  result <- action
  after <- IO.liftIO Clock.getMonotonicTimeNSec
  pure (result, after - before)

sendExceptionToDiscord :: Exception.SomeException -> App.App request a
sendExceptionToDiscord exception = do
  Console.warn $ Exception.displayException exception
  context <- Reader.ask
  Trans.lift $ Settings.sendExceptionToDiscord context exception
  Exception.throwM exception

-- TODO: This method of pruning blobs is way too time consuming.
doNot :: Applicative m => m a -> m ()
doNot = const $ pure ()

pruneBlobs :: App.App request ()
pruneBlobs = doNot $ do
  rows <- App.sql
    "select blobs.sha256 \
    \from blobs \
    \left join files \
    \on files.digest = blobs.sha256 \
    \where files.digest is null"
    ()
  let count = length rows
  if count == 0
    then Console.info "Did not find any orphaned blobs."
    else do
      Console.info $ unwords ["Pruning", pluralize "orphan blob" count, "..."]
      mapM_
        (App.sql_ "delete from blobs where sha256 = ?")
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
  upsertBlob_ body sha256
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
indexPath = Path.fromFilePath "index.tar.gz"

handle304 :: App.App request ()
handle304 = Console.info "Hackage index has not changed."

handleOther
  :: Client.Request
  -> Client.Response LazyByteString.ByteString
  -> App.App request a
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
      indexUrl <- getIndexUrl
      Console.info $ mconcat ["Missing SHA256 for ", show indexUrl, "."]
      removeCache
    Just sha256 -> maybeProcess_ indexPath sha256 $ do
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

processIndexWith :: Binary.Binary -> App.App request ()
processIndexWith binary = do
  countVar <- Trans.lift $ Stm.newTVarIO 1
  revisionsVar <- Trans.lift $ Stm.newTVarIO Map.empty
  versionsVar <- Trans.lift $ Stm.newTVarIO Map.empty
  mapM_ (processIndexEntry countVar revisionsVar versionsVar)
    . Tar.foldEntries (:) [] handleFormatError
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

processIndexEntry
  :: Stm.TVar Word
  -> Stm.TVar
       ( Map.Map
           PackageName.PackageName
           (Map.Map Version.Version Revision.Revision)
       )
  -> Stm.TVar (Map.Map PackageName.PackageName VersionRange.VersionRange)
  -> Tar.Entry
  -> App.App request ()
processIndexEntry countVar revisionsVar versionsVar entry = do
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
    [rawPackageName, "preferred-versions"] -> parsePackageName rawPackageName
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

parsePackageName
  :: Exception.MonadThrow m => String -> m PackageName.PackageName
parsePackageName string = case PackageName.fromString string of
  Nothing -> WithCallStack.throw $ InvalidPackageName string
  Just packageName -> pure packageName

processPackageDescription
  :: Stm.TVar
       ( Map.Map
           PackageName.PackageName
           (Map.Map Version.Version Revision.Revision)
       )
  -> Path.Path
  -> ByteString.ByteString
  -> Sha256.Sha256
  -> App.App request ()
processPackageDescription revisionsVar path strictContents digest = do
  -- Get the package name and version from the path.
  (packageName, version) <- case Path.toStrings path of
    [rawPackageName, rawVersion, _] -> do
      packageName <- parsePackageName rawPackageName
      version <- parseVersion rawVersion
      pure (packageName, version)
    strings -> WithCallStack.throw $ UnexpectedPath strings
  -- Get the revision number and update the map.
  revision <- Trans.lift . Stm.atomically $ do
    allRevisions <- Stm.readTVar revisionsVar
    case Map.lookup packageName allRevisions of
      Nothing -> do
        let revision = Revision.zero
        Stm.modifyTVar revisionsVar . Map.insert packageName $ Map.singleton
          version
          revision
        pure revision
      Just packageRevisions -> case Map.lookup version packageRevisions of
        Nothing -> do
          let revision = Revision.zero
          Stm.modifyTVar revisionsVar
            $ Map.adjust (Map.insert version revision) packageName
          pure revision
        Just versionRevision -> do
          let revision = Revision.increment versionRevision
          Stm.modifyTVar revisionsVar
            $ Map.adjust (Map.insert version revision) packageName
          pure revision
  -- Build the new path with revision.
  let
    newPath = Path.fromStrings
      [ "d"
      , PackageName.toString packageName
      , Version.toString version
      , Revision.toString revision
      , ".cabal"
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
  upsertBlob_ strictContents digest
  App.sql_
    "insert into files (digest, name) values (?, ?) \
    \on conflict (name) do update set digest = excluded.digest"
    (digest, newPath)

parseVersion :: Exception.MonadThrow m => String -> m Version.Version
parseVersion string = case Cabal.simpleParsec string of
  Nothing -> WithCallStack.throw $ InvalidVersionNumber string
  Just version -> pure $ Version.fromCabal version

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
  :: Path.Path -> ByteString.ByteString -> Sha256.Sha256 -> App.App request ()
processPackageSignature path strictContents digest = do
  (packageName, version) <- case Path.toStrings path of
    [rawPackageName, rawVersion, "package.json"] -> do
      packageName <- parsePackageName rawPackageName
      version <- parseVersion rawVersion
      pure (packageName, version)
    strings -> WithCallStack.throw $ UnexpectedPath strings
  let
    newPath = Path.fromStrings
      [ "s"
      , PackageName.toString packageName
      , Version.toString version
      , ".json"
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
  upsertBlob_ strictContents digest
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

upsertBlob_ :: ByteString.ByteString -> Sha256.Sha256 -> App.App request ()
upsertBlob_ contents = Monad.void . upsertBlob contents

upsertBlob :: ByteString.ByteString -> Sha256.Sha256 -> App.App request Bool
upsertBlob contents sha256 = do
  rows <- App.sql "select count(*) from blobs where sha256 = ?" [sha256]
  let count = maybe (0 :: Int) Sql.fromOnly $ Maybe.listToMaybe rows
  if count < 1
    then do
      App.sql_
        "insert into blobs (octets, sha256, size) values (?, ?, ?)"
        ( Binary.fromByteString contents
        , sha256
        , Size.fromInt $ ByteString.length contents
        )
      pure True
    else pure False

newtype UnknownExtension
  = UnknownExtension Tar.Entry
  deriving (Eq, Show)

instance Exception.Exception UnknownExtension

newtype UnknownEntry
  = UnknownEntry Tar.Entry
  deriving (Eq, Show)

instance Exception.Exception UnknownEntry

unsafeThrow :: Exception.Exception e => e -> a
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

fetchTarballs :: App.App request ()
fetchTarballs = do
  names <- App.sql
    "select name from files where name like 'd/%/%/0/.cabal' order by name asc"
    ()
  mapM_ (fetchTarball . Sql.fromOnly) names

fetchTarball :: Path.Path -> App.App request ()
fetchTarball path = do
  (package, version) <- case Path.toStrings path of
    ["d", rawPackage, rawVersion, "0", ".cabal"] -> do
      package <- parsePackageName rawPackage
      version <- parseVersion rawVersion
      pure (PackageName.toString package, Version.toString version)
    strings -> WithCallStack.throw $ UnexpectedPath strings
  let tarballPath = Path.fromStrings ["t", package, version, ".tar.gz"]
  fileRows <- App.sql "select count(*) from files where name = ?" [tarballPath]
  case fileRows of
    Sql.Only count : _ | count > (0 :: Int) -> pure ()
    _ -> do
      sha256 <- do
        oldFileRows <- App.sql
          "select digest from files where name = ?"
          [Path.fromStrings [package, version, package <> ".tar.gz"]]
        case oldFileRows of
          Sql.Only sha256 : _ -> pure sha256
          _ -> do
            hackageUrl <- Reader.asks $ Config.hackageUrl . Context.config
            let
              pkg = mconcat [package, "-", version]
              url =
                mconcat [hackageUrl, "/package/", pkg, "/", pkg, ".tar.gz"]
            initialRequest <- Client.parseRequest url
            cacheRows <- App.sql "select etag from cache where url = ?" [url]
            let
              etag = case cacheRows of
                Sql.Only x : _ -> x
                _ -> Etag.fromByteString ByteString.empty
              request = addRequestHeader
                Http.hIfNoneMatch
                (Etag.toByteString etag)
                initialRequest
            response <- getResponse request
            body <- case Http.statusCode $ Client.responseStatus response of
              200 -> do
                Console.info $ unwords ["Downloaded", pkg, "tarball."]
                pure . LazyByteString.toStrict $ Client.responseBody response
              410 -> do
                Console.warn $ unwords ["Tarball", pkg, "gone!"]
                pure emptyTarball
              451 -> do
                Console.warn $ unwords ["Tarball", pkg, "unavailable!"]
                pure emptyTarball
              _ -> handleOther request response
            let
              newEtag =
                Etag.fromByteString
                  . Maybe.fromMaybe ByteString.empty
                  . lookup Http.hETag
                  $ Client.responseHeaders response
              sha256 = Sha256.fromDigest $ Crypto.hash body
            upsertBlob_ body sha256
            App.sql_
              "insert into cache (etag, sha256, url) values (?, 'unused', ?)"
              (newEtag, url)
            pure sha256
      App.sql_
        "insert into files (digest, name) values (?, ?) \
        \ on conflict (name) do update set \
        \ digest = excluded.digest"
        (sha256, tarballPath)

-- | Some packages exist in the index even though their tarballs aren't
-- available. For example:
--
-- - Trying to get the tarball for package @Clash-Royale-Hack-Cheats@ version
--   @1.0.1@ returns an HTTP 410 Gone response.
--   <https://github.com/haskell-infra/hackage-trustees/issues/132>
--
-- - Similarly the package @hermes@ (nominally version @1.3.4.3@) has been
--   removed "for legal reasons" and returns an HTTP 451.
--   <https://github.com/haskell/hackage-server/issues/436>
emptyTarball :: ByteString.ByteString
emptyTarball = LazyByteString.toStrict . Gzip.compress $ Tar.write []

processTarballs :: App.App request ()
processTarballs = do
  countVar <- Trans.lift $ Stm.newTVarIO 1
  rows <- App.sql
    "select name, digest \
    \from files \
    \where name like 't/%/%/.tar.gz' \
    \order by name asc"
    ()
  mapM_ (uncurry $ processTarball countVar) rows

processTarball
  :: Stm.TVar Word -> Path.Path -> Sha256.Sha256 -> App.App request ()
processTarball countVar path sha256 = maybeProcess_ path sha256 $ do
  count <- Trans.lift . Stm.atomically $ do
    count <- Stm.readTVar countVar
    Stm.modifyTVar countVar (+ 1)
    pure count
  Monad.when (rem count 1000 == 0)
    . Console.info
    . unwords
    $ ["Processing tarball number", show count, "..."]
  (package, version) <- case Path.toStrings path of
    ["t", rawPackage, rawVersion, ".tar.gz"] -> do
      package <- parsePackageName rawPackage
      version <- parseVersion rawVersion
      pure (package, version)
    strings -> WithCallStack.throw $ UnexpectedPath strings
  binary <- do
    maybeBinary <- getBinary sha256
    case maybeBinary of
      Nothing -> WithCallStack.throw $ MissingBinary sha256
      Just binary -> pure binary
  linkVar <- Trans.lift Stm.newEmptyTMVarIO
  mapM_ (processTarballEntry package version linkVar)
    . Tar.foldEntries (:) [] handleFormatError
    . Tar.read
    . Gzip.decompress
    . LazyByteString.fromStrict
    $ Binary.toByteString binary

-- https://github.com/haskell/hackage-server/issues/851
handleFormatError :: Tar.FormatError -> [a]
handleFormatError formatError = case formatError of
  Tar.ShortTrailer -> []
  _ -> unsafeThrow formatError

processTarballEntry
  :: PackageName.PackageName
  -> Version.Version
  -> Stm.TMVar Path.Path
  -> Tar.Entry
  -> App.App request ()
processTarballEntry package version linkVar entry =
  case Tar.entryContent entry of

    Tar.OtherEntryType 'L' byteString _ -> do
      let
        expected = Path.fromStrings [".", ".", "@LongLink"]
        actual = Path.fromFilePath $ Tar.entryPath entry
        link =
          Path.fromFilePath
            . Utf8.toString
            . stripTrailingNullBytes
            $ LazyByteString.toStrict byteString
      Monad.when (actual /= expected) . WithCallStack.throw $ InvalidLongLink
        entry
      Trans.lift . Stm.atomically $ do
        maybeLink <- Stm.tryTakeTMVar linkVar
        case maybeLink of
          Just oldLink -> WithCallStack.throw $ LongLinkOverwrite oldLink link
          Nothing -> Stm.putTMVar linkVar link

    Tar.NormalFile byteString _ -> do
      let entryPath = Tar.entryPath entry
      maybeLink <- Trans.lift . Stm.atomically $ Stm.tryTakeTMVar linkVar
      partialPath <- case maybeLink of
        Nothing -> pure $ Path.fromFilePath entryPath
        Just link -> do
          let
            prefix = Path.toFilePath $ Path.fromFilePath entryPath
            string = Path.toFilePath link
          Monad.unless (prefix `List.isPrefixOf` string)
            . WithCallStack.throw
            $ InvalidPrefix prefix string
          pure link
      let
        contents = LazyByteString.toStrict byteString
        sha256 = Sha256.fromDigest $ Crypto.hash contents
        prefix = mconcat
          [PackageName.toString package, "-", Version.toString version, "/"]
        string = Path.toFilePath partialPath
      if prefix `List.isPrefixOf` string
        then do
          upsertBlob_ contents sha256
          let
            fullPath =
              Path.fromStrings
                . ("c" :)
                . (PackageName.toString package :)
                . (Version.toString version :)
                . drop 1
                $ Path.toStrings partialPath
          App.sql_
            "insert into files (digest, name) values (?, ?) \
            \on conflict (name) do update set digest = excluded.digest"
            (sha256, fullPath)
        else
          Monad.when (Set.notMember entryPath ignoredPaths)
          . WithCallStack.throw
          $ InvalidPrefix prefix string

    Tar.Directory -> pure ()
    Tar.HardLink _ -> pure () -- https://github.com/haskell/hackage-server/issues/858
    Tar.OtherEntryType '5' _ _ -> pure () -- directory
    Tar.OtherEntryType 'g' _ _ -> pure () -- pax_global_header
    Tar.OtherEntryType 'x' _ _ -> pure () -- metadata
    Tar.SymbolicLink _ -> pure ()

    _ -> WithCallStack.throw $ UnknownEntry entry

-- | Some old package tarballs contain paths that don't start with the package
-- identifier. In general we want to throw an exception if we encounter a path
-- with the wrong prefix. Since these paths already exist, we don't want to
-- throw and exception for them.
ignoredPaths :: Set.Set FilePath
ignoredPaths = Set.fromList $ fmap
  FilePath.joinPath
  [ [".", "._BirdPP-1.1"]
  , [".", "._ParserFunction-0.0.4"]
  , [".", "._ParserFunction-0.0.5"]
  , ["cabal-sign-0.1.0.0.sig"]
  , ["cabal-sign-0.2.0.0.sig"]
  , [".", "._mtlx-0.1.1"]
  , [".", "._mtlx-0.1"]
  ]

stripTrailingNullBytes :: ByteString.ByteString -> ByteString.ByteString
stripTrailingNullBytes = fst . ByteString.spanEnd (== 0x00)

newtype MissingBinary
  = MissingBinary Sha256.Sha256
  deriving (Eq, Show)

instance Exception.Exception MissingBinary

newtype InvalidLongLink
  = InvalidLongLink Tar.Entry
  deriving (Eq, Show)

instance Exception.Exception InvalidLongLink

data LongLinkOverwrite
  = LongLinkOverwrite Path.Path Path.Path
  deriving (Eq, Show)

instance Exception.Exception LongLinkOverwrite

data InvalidPrefix
  = InvalidPrefix FilePath FilePath
  deriving (Eq, Show)

instance Exception.Exception InvalidPrefix

parsePackageDescriptions :: App.App request ()
parsePackageDescriptions = do
  countVar <- Trans.lift $ Stm.newTVarIO 1
  rows <- App.sql
    "select name, digest \
    \from files \
    \where name like 'd/%/%/%/.cabal' \
    \order by name asc"
    ()
  mapM_ (uncurry $ parsePackageDescription countVar) rows

parsePackageDescription
  :: Stm.TVar Word -> Path.Path -> Sha256.Sha256 -> App.App request ()
parsePackageDescription countVar path sha256 = maybeProcess_ path sha256 $ do
  count <- Trans.lift . Stm.atomically $ do
    count <- Stm.readTVar countVar
    Stm.modifyTVar countVar (+ 1)
    pure count
  Monad.when (rem count 1000 == 0)
    . Console.info
    . unwords
    $ ["Parsing package description number", show count, "..."]
  (pkg, ver, rev) <- case Path.toStrings path of
    ["d", rawPackage, rawVersion, rawRevision, ".cabal"] -> do
      package <- parsePackageName rawPackage
      version <- parseVersion rawVersion
      revision <- parseRevision rawRevision
      pure (package, version, revision)
    strings -> WithCallStack.throw $ UnexpectedPath strings
  binary <- do
    maybeBinary <- getBinary sha256
    case maybeBinary of
      Nothing -> WithCallStack.throw $ MissingBinary sha256
      Just binary -> pure binary
  case Monadoc.Utility.Cabal.parse $ Binary.toByteString binary of
    Left errs -> WithCallStack.throw . userError $ show (pkg, ver, rev, errs)
    Right package -> do
      let
        packageName =
          Cabal.pkgName
            . Cabal.package
            . Cabal.packageDescription
            $ Monadoc.Utility.Cabal.unwrapPackage package
        versionNumber =
          Version.fromCabal
            . Cabal.pkgVersion
            . Cabal.package
            . Cabal.packageDescription
            $ Monadoc.Utility.Cabal.unwrapPackage package
      Monad.when (packageName /= PackageName.toCabal pkg)
        . WithCallStack.throw
        $ PackageNameMismatch pkg packageName
      Monad.when (versionNumber /= ver)
        . WithCallStack.throw
        $ VersionNumberMismatch ver versionNumber
      -- We don't bother checking the revision against the x-revision field
      -- because it's often wrong. See these Hackage issues for details:
      -- <https://github.com/haskell/hackage-server/issues/337>
      -- <https://github.com/haskell/hackage-server/issues/779>

      -- One package can potentially have many public sub-libraries, but
      -- this feature isn't well supported yet. See this issue for details:
      -- <https://github.com/haskell/cabal/issues/5660>.
      case toPackageDescription package of
        Left _ -> WithCallStack.throw . userError $ show (pkg, ver, rev)
        Right (pacdes, _) -> case Cabal.library pacdes of
          Nothing -> pure ()
          Just library -> do
            let
              buildInfo = Cabal.libBuildInfo library
              sourceDirs = Cabal.hsSourceDirs buildInfo
              extensions =
                Maybe.mapMaybe
                    (\x -> case x of
                      C.UnknownExtension _ -> Nothing
                      C.EnableExtension y -> do
                        z <- convertExtension y
                        pure (True, z)
                      C.DisableExtension y -> do
                        z <- convertExtension y
                        pure (False, z)
                    )
                  $ Cabal.defaultExtensions buildInfo
                  <> Cabal.oldExtensions buildInfo
            Monad.forM_
                (fmap ModuleName.fromCabal $ Cabal.exposedModules library)
              $ \moduleName -> do
                  maybeFile <- findSourceFile pkg ver sourceDirs moduleName
                  App.sql_
                    "insert into exposed_modules \
                    \(package, version, revision, module, file) \
                    \values (?, ?, ?, ?, ?) \
                    \on conflict (package, version, revision, module) \
                    \do update set file = excluded.file"
                    (pkg, ver, rev, moduleName, maybeFile)
                  let
                    key = unwords
                      [ PackageName.toString pkg
                      , Version.toString ver
                      , Revision.toString rev
                      , ModuleName.toString moduleName
                      ]
                  case maybeFile of
                    Nothing -> Console.info $ "SKIP " <> key
                    Just file -> do
                      rows <- App.sql
                        "select blobs.octets from blobs \
                        \inner join files on files.digest = blobs.sha256 \
                        \where files.name = ?"
                        [file]
                      case rows of
                        [] -> fail $ "missing contents for " <> show file
                        Sql.Only contents : _ -> do
                          let _ = contents :: Binary.Binary
                          result <- IO.liftIO $ Monadoc.Ghc.parse
                            extensions
                            (Path.toFilePath file)
                            (Binary.toByteString contents)
                          App.sql_
                            "update exposed_modules \
                            \set parsed = ? \
                            \where package = ? \
                            \and version = ? \
                            \and revision = ? \
                            \and module = ?"
                            (Either.isRight result, pkg, ver, rev, moduleName)
                          case result of
                            Left _ -> Console.info $ "FAIL " <> key
                            Right _ -> Console.info $ "PASS " <> key

convertExtension :: C.KnownExtension -> Maybe G.Extension
convertExtension x = case x of
  C.AllowAmbiguousTypes -> Just G.AllowAmbiguousTypes
  C.ApplicativeDo -> Just G.ApplicativeDo
  C.Arrows -> Just G.Arrows
  C.AutoDeriveTypeable -> Just G.AutoDeriveTypeable
  C.BangPatterns -> Just G.BangPatterns
  C.BinaryLiterals -> Just G.BinaryLiterals
  C.BlockArguments -> Just G.BlockArguments
  C.CApiFFI -> Just G.CApiFFI
  C.ConstrainedClassMethods -> Just G.ConstrainedClassMethods
  C.ConstraintKinds -> Just G.ConstraintKinds
  C.CPP -> Just G.Cpp
  C.CUSKs -> Just G.CUSKs
  C.DataKinds -> Just G.DataKinds
  C.DatatypeContexts -> Just G.DatatypeContexts
  C.DefaultSignatures -> Just G.DefaultSignatures
  C.DeriveAnyClass -> Just G.DeriveAnyClass
  C.DeriveDataTypeable -> Just G.DeriveDataTypeable
  C.DeriveFoldable -> Just G.DeriveFoldable
  C.DeriveFunctor -> Just G.DeriveFunctor
  C.DeriveGeneric -> Just G.DeriveGeneric
  C.DeriveLift -> Just G.DeriveLift
  C.DeriveTraversable -> Just G.DeriveTraversable
  C.DerivingStrategies -> Just G.DerivingStrategies
  C.DerivingVia -> Just G.DerivingVia
  C.DisambiguateRecordFields -> Just G.DisambiguateRecordFields
  C.DoAndIfThenElse -> Just G.DoAndIfThenElse
  C.DoRec -> Just G.RecursiveDo
  C.DuplicateRecordFields -> Just G.DuplicateRecordFields
  C.EmptyCase -> Just G.EmptyCase
  C.EmptyDataDecls -> Just G.EmptyDataDecls
  C.EmptyDataDeriving -> Just G.EmptyDataDeriving
  C.ExistentialQuantification -> Just G.ExistentialQuantification
  C.ExplicitForAll -> Just G.ExplicitForAll
  C.ExplicitNamespaces -> Just G.ExplicitNamespaces
  C.ExtendedDefaultRules -> Just G.ExtendedDefaultRules
  C.FlexibleContexts -> Just G.FlexibleContexts
  C.FlexibleInstances -> Just G.FlexibleInstances
  C.ForeignFunctionInterface -> Just G.ForeignFunctionInterface
  C.FunctionalDependencies -> Just G.FunctionalDependencies
  C.GADTs -> Just G.GADTs
  C.GADTSyntax -> Just G.GADTSyntax
  C.GeneralisedNewtypeDeriving -> Just G.GeneralizedNewtypeDeriving
  C.GeneralizedNewtypeDeriving -> Just G.GeneralizedNewtypeDeriving
  C.GHCForeignImportPrim -> Just G.GHCForeignImportPrim
  C.HexFloatLiterals -> Just G.HexFloatLiterals
  C.ImplicitParams -> Just G.ImplicitParams
  C.ImplicitPrelude -> Just G.ImplicitPrelude
  C.ImportQualifiedPost -> Just G.ImportQualifiedPost
  C.ImpredicativeTypes -> Just G.ImpredicativeTypes
  C.IncoherentInstances -> Just G.IncoherentInstances
  C.InstanceSigs -> Just G.InstanceSigs
  C.InterruptibleFFI -> Just G.InterruptibleFFI
  C.JavaScriptFFI -> Just G.JavaScriptFFI
  C.KindSignatures -> Just G.KindSignatures
  C.LambdaCase -> Just G.LambdaCase
  C.LiberalTypeSynonyms -> Just G.LiberalTypeSynonyms
  C.MagicHash -> Just G.MagicHash
  C.MonadComprehensions -> Just G.MonadComprehensions
  C.MonadFailDesugaring -> Just G.MonadFailDesugaring
  C.MonoLocalBinds -> Just G.MonoLocalBinds
  C.MonomorphismRestriction -> Just G.MonomorphismRestriction
  C.MonoPatBinds -> Just G.MonoPatBinds
  C.MultiParamTypeClasses -> Just G.MultiParamTypeClasses
  C.MultiWayIf -> Just G.MultiWayIf
  C.NamedFieldPuns -> Just G.RecordPuns
  C.NamedWildCards -> Just G.NamedWildCards
  C.NegativeLiterals -> Just G.NegativeLiterals
  C.NondecreasingIndentation -> Just G.NondecreasingIndentation
  C.NPlusKPatterns -> Just G.NPlusKPatterns
  C.NullaryTypeClasses -> Just G.NullaryTypeClasses
  C.NumDecimals -> Just G.NumDecimals
  C.NumericUnderscores -> Just G.NumericUnderscores
  C.OverlappingInstances -> Just G.OverlappingInstances
  C.OverloadedLabels -> Just G.OverloadedLabels
  C.OverloadedLists -> Just G.OverloadedLists
  C.OverloadedStrings -> Just G.OverloadedStrings
  C.PackageImports -> Just G.PackageImports
  C.ParallelArrays -> Just G.ParallelArrays
  C.ParallelListComp -> Just G.ParallelListComp
  C.PartialTypeSignatures -> Just G.PartialTypeSignatures
  C.PatternGuards -> Just G.PatternGuards
  C.PatternSignatures -> Just G.ScopedTypeVariables
  C.PatternSynonyms -> Just G.PatternSynonyms
  C.PolyKinds -> Just G.PolyKinds
  C.PolymorphicComponents -> Just G.RankNTypes
  C.PostfixOperators -> Just G.PostfixOperators
  C.QuantifiedConstraints -> Just G.QuantifiedConstraints
  C.QuasiQuotes -> Just G.QuasiQuotes
  C.Rank2Types -> Just G.RankNTypes
  C.RankNTypes -> Just G.RankNTypes
  C.RebindableSyntax -> Just G.RebindableSyntax
  C.RecordPuns -> Just G.RecordPuns
  C.RecordWildCards -> Just G.RecordWildCards
  C.RecursiveDo -> Just G.RecursiveDo
  C.RelaxedPolyRec -> Just G.RelaxedPolyRec
  C.RoleAnnotations -> Just G.RoleAnnotations
  C.ScopedTypeVariables -> Just G.ScopedTypeVariables
  C.StandaloneDeriving -> Just G.StandaloneDeriving
  C.StandaloneKindSignatures -> Just G.StandaloneKindSignatures
  C.StarIsType -> Just G.StarIsType
  C.StaticPointers -> Just G.StaticPointers
  C.Strict -> Just G.Strict
  C.StrictData -> Just G.StrictData
  C.TemplateHaskell -> Just G.TemplateHaskell
  C.TemplateHaskellQuotes -> Just G.TemplateHaskellQuotes
  C.TraditionalRecordSyntax -> Just G.TraditionalRecordSyntax
  C.TransformListComp -> Just G.TransformListComp
  C.TupleSections -> Just G.TupleSections
  C.TypeApplications -> Just G.TypeApplications
  C.TypeFamilies -> Just G.TypeFamilies
  C.TypeFamilyDependencies -> Just G.TypeFamilyDependencies
  C.TypeInType -> Just G.TypeInType
  C.TypeOperators -> Just G.TypeOperators
  C.TypeSynonymInstances -> Just G.TypeSynonymInstances
  C.UnboxedSums -> Just G.UnboxedSums
  C.UnboxedTuples -> Just G.UnboxedTuples
  C.UndecidableInstances -> Just G.UndecidableInstances
  C.UndecidableSuperClasses -> Just G.UndecidableSuperClasses
  C.UnicodeSyntax -> Just G.UnicodeSyntax
  C.UnliftedFFITypes -> Just G.UnliftedFFITypes
  C.UnliftedNewtypes -> Just G.UnliftedNewtypes
  C.ViewPatterns -> Just G.ViewPatterns

  C.ExtensibleRecords -> Nothing
  C.Generics -> Nothing
  C.HereDocuments -> Nothing
  C.NewQualifiedOperators -> Nothing
  C.RegularPatterns -> Nothing
  C.RestrictedTypeSynonyms -> Nothing
  C.Safe -> Nothing
  C.SafeImports -> Nothing
  C.Trustworthy -> Nothing
  C.Unsafe -> Nothing
  C.XmlSyntax -> Nothing

findSourceFile
  :: PackageName.PackageName
  -> Version.Version
  -> [FilePath]
  -> ModuleName.ModuleName
  -> App.App request (Maybe Path.Path)
findSourceFile pkg ver dirs mdl = case dirs of
  [] -> findSourceFileIn pkg ver "." mdl
  _ -> mapMaybeM (\dir -> findSourceFileIn pkg ver dir mdl) dirs

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
mapMaybeM f l = case l of
  [] -> pure Nothing
  h : t -> do
    m <- f h
    case m of
      Just x -> pure $ Just x
      Nothing -> mapMaybeM f t

findSourceFileIn
  :: PackageName.PackageName
  -> Version.Version
  -> FilePath
  -> ModuleName.ModuleName
  -> App.App request (Maybe Path.Path)
findSourceFileIn pkg ver dir mdl = mapMaybeM
  (findSourceFileWith pkg ver dir mdl)
  ["hs", "hsc", "lhs", "chs", "x", "y", "cpphs", "xhs", "xpphs", "gc"]

findSourceFileWith
  :: PackageName.PackageName
  -> Version.Version
  -> FilePath
  -> ModuleName.ModuleName
  -> String
  -> App.App request (Maybe Path.Path)
findSourceFileWith pkg ver dir mdl ext = do
  let
    path = mconcat
      [ Path.fromStrings ["c", PackageName.toString pkg, Version.toString ver]
      , case dir of
        "." -> mempty
        "./" -> mempty
        "./." -> mempty
        '.' : '/' : rest -> Path.fromFilePath rest
        _ -> Path.fromFilePath dir
      , Path.fromFilePath
      $ Cabal.toFilePath (ModuleName.toCabal mdl)
      <> "."
      <> ext
      ]
  rows <- App.sql "select name from files where name = ?" [path]
  case rows of
    [] -> pure Nothing
    Sql.Only name : _ -> pure $ Just name

-- | Although the generic package description type does have a package
-- description in it, that nested PD isn't actually usable. This function is
-- necessary in order to choose the platform, compiler, flags, and other stuff.
toPackageDescription
  :: Monadoc.Utility.Cabal.Package
  -> Either
       [Cabal.Dependency]
       (Cabal.PackageDescription, Cabal.FlagAssignment)
toPackageDescription =
  let
    flagAssignment = Cabal.mkFlagAssignment []
    componentRequestedSpec = Cabal.ComponentRequestedSpec False False
    isDependencySatisfiable = const True
    platform = Cabal.Platform Cabal.X86_64 Cabal.Linux
    compilerId = Cabal.CompilerId Cabal.GHC $ Cabal.mkVersion [8, 10, 1]
    abiTag = Cabal.NoAbiTag
    compilerInfo = Cabal.unknownCompilerInfo compilerId abiTag
    additionalConstraints = []
  in
    Cabal.finalizePD
        flagAssignment
        componentRequestedSpec
        isDependencySatisfiable
        platform
        compilerInfo
        additionalConstraints
      . Monadoc.Utility.Cabal.unwrapPackage

data VersionNumberMismatch
  = VersionNumberMismatch Version.Version Version.Version
  deriving (Eq, Show)

instance Exception.Exception VersionNumberMismatch

parseRevision :: Exception.MonadThrow m => String -> m Revision.Revision
parseRevision string = case Revision.fromString string of
  Nothing -> WithCallStack.throw $ InvalidRevision string
  Just revision -> pure revision

newtype InvalidRevision
  = InvalidRevision String
  deriving (Eq, Show)

instance Exception.Exception InvalidRevision

maybeProcess_
  :: Path.Path -> Sha256.Sha256 -> App.App request () -> App.App request ()
maybeProcess_ path sha256 = Monad.void . maybeProcess path sha256

maybeProcess
  :: Path.Path
  -> Sha256.Sha256
  -> App.App request a
  -> App.App request (Maybe a)
maybeProcess path sha256 process = do
  rows <- App.sql "select sha256 from processed_files where path = ?" [path]
  case rows of
    Sql.Only actual : _ | actual == sha256 -> pure Nothing
    _ -> do
      result <- process
      timestamp <- IO.liftIO $ fmap Timestamp.fromUtcTime Time.getCurrentTime
      App.sql_
        "insert into processed_files (path, sha256, timestamp) \
        \values (?, ?, ?) on conflict (path) do update set \
        \sha256 = excluded.sha256, timestamp = excluded.timestamp"
        (path, sha256, timestamp)
      pure $ Just result
