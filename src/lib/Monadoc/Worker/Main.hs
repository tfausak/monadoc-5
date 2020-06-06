module Monadoc.Worker.Main
  ( run
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
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

withConnection :: (Sql.Connection -> App.App a) -> App.App a
withConnection app = do
  pool <- Reader.asks Context.pool
  Pool.withResource pool app

run :: App.App ()
run = Monad.forever $ do
  Console.info "updating hackage index"
  let url = "https://hackage.haskell.org/01-index.tar.gz"
  result <- withConnection $ \connection -> Trans.lift $ Sql.query
    connection
    (Sql.sql "select etag, sha256 from cache where url = ?")
    [url]
  contents <- case result of
    [] -> do
      Console.info "index is not cached"
      request <- Client.parseRequest url
      manager <- Reader.asks Context.manager
      response <- Trans.lift $ Client.httpLbs request manager
      Monad.when (Client.responseStatus response /= Http.ok200)
        . WithCallStack.throw
        . userError
        $ show response
      let
        body = LazyByteString.toStrict $ Client.responseBody response
        sha256 = Sha256.fromDigest $ Crypto.hash body
        etag =
          Etag.fromByteString
            . Maybe.fromMaybe ByteString.empty
            . lookup Http.hETag
            $ Client.responseHeaders response
      Console.info "got index, caching response"
      withConnection $ \connection -> Trans.lift $ do
        Sql.execute
          connection
          (Sql.sql
            "insert into blobs (octets, sha256, size) \
            \ values (?, ?, ?) on conflict (sha256) do nothing"
          )
          ( Binary.fromByteString body
          , sha256
          , Size.fromInt $ ByteString.length body
          )
        Sql.execute
          connection
          (Sql.sql
            "insert into cache (etag, sha256, url) values (?, ?, ?) \
            \ on conflict (url) do update set \
            \ etag = excluded.etag, sha256 = excluded.sha256"
          )
          (etag, sha256, url)
      pure body
    (etag, sha256) : _ -> do
      Console.info "index is cached"
      request <- addRequestHeader Http.hIfNoneMatch (Etag.toByteString etag)
        <$> Client.parseRequest url
      manager <- Reader.asks Context.manager
      response <- Trans.lift $ Client.httpLbs request manager
      case Http.statusCode $ Client.responseStatus response of
        200 -> do
          Console.info "index has changed"
          let
            body = LazyByteString.toStrict $ Client.responseBody response
            newSha256 = Sha256.fromDigest $ Crypto.hash body
            newEtag =
              Etag.fromByteString
                . Maybe.fromMaybe ByteString.empty
                . lookup Http.hETag
                $ Client.responseHeaders response
          Console.info "got index, caching response"
          withConnection $ \connection -> Trans.lift $ do
            Sql.execute
              connection
              (Sql.sql
                "insert into blobs (octets, sha256, size) \
                \ values (?, ?, ?) on conflict (sha256) do nothing"
              )
              ( Binary.fromByteString body
              , newSha256
              , Size.fromInt $ ByteString.length body
              )
            Sql.execute
              connection
              (Sql.sql
                "insert into cache (etag, sha256, url) values (?, ?, ?) \
                \ on conflict (url) do update set \
                \ etag = excluded.etag, sha256 = excluded.sha256"
              )
              (newEtag, newSha256, url)
          pure body
        304 -> do
          Console.info "index has not changed"
          rows <- withConnection $ \connection -> Trans.lift $ Sql.query
            connection
            (Sql.sql "select octets from blobs where sha256 = ?")
            [sha256 :: Sha256.Sha256]
          case rows of
            [] -> WithCallStack.throw $ userError "missing index blob"
            row : _ -> pure . Binary.toByteString $ Sql.fromOnly row
        _ -> WithCallStack.throw . userError $ show response
  Console.info $ "index size: " <> show (ByteString.length contents)
  Console.info
    . mappend "index entry count: "
    . show
    . length
    . Tar.foldEntries
        (\entry entries -> case Tar.entryContent entry of
          Tar.NormalFile _contents _size ->
            case FilePath.takeExtension $ Tar.entryPath entry of
              "" -> entry : entries -- preferred-versions
              ".cabal" -> entry : entries
              ".json" -> entries -- ignore
              _ ->
                Unsafe.unsafePerformIO . WithCallStack.throw . userError $ show
                  entry
          _ -> Unsafe.unsafePerformIO . WithCallStack.throw . userError $ show
            entry
        )
        []
        (Unsafe.unsafePerformIO . WithCallStack.throw)
    . Tar.read
    . Gzip.decompress
    $ LazyByteString.fromStrict contents
  Trans.lift $ Concurrent.threadDelay 60000000

addRequestHeader
  :: Http.HeaderName
  -> ByteString.ByteString
  -> Client.Request
  -> Client.Request
addRequestHeader name value request = request
  { Client.requestHeaders = (name, value) : Client.requestHeaders request
  }
