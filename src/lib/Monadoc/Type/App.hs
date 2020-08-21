module Monadoc.Type.App where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Pool as Pool
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Sql as Sql
import qualified Test.Hspec as Hspec

-- | The main application type. This simply provides the run-time context. Use
-- 'run' to convert this into 'IO'.
type App request result = Reader.ReaderT (Context.Context request) IO result

-- | Runs an 'App' action.
run :: Context.Context request -> App request result -> IO result
run = flip Reader.runReaderT

-- | Runs a SQL query and returns the results.
sql :: (Sql.FromRow b, Sql.ToRow a) => Sql.Query -> a -> App request [b]
sql query params = withConnection
  $ \connection -> Trans.lift $ Sql.query connection query params

-- | Runs a SQL query and discards the results.
sql_ :: Sql.ToRow a => Sql.Query -> a -> App request ()
sql_ query params =
  Monad.void (sql query params :: App request [[Sql.SQLData]])

-- | Checks out a SQL connection from the pool and runs the given action with
-- it.
withConnection :: (Sql.Connection -> App request result) -> App request result
withConnection action = do
  pool <- Reader.asks Context.pool
  Pool.withResource pool action

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.App" $ do

  Hspec.describe "run" $ do

    Hspec.it "works" $ do
      Hspec.pending
      -- ctx <- makeContext
      -- run ctx (pure ()) `Hspec.shouldReturn` ()

  Hspec.describe "withConnection" $ do

    Hspec.it "works" $ do
      Hspec.pending
      -- ctx <- makeContext
      -- result <- run ctx . withConnection $ \connection ->
      --   IO.liftIO $ Sql.query_ connection "select 1"
      -- result `Hspec.shouldBe` [Sql.Only (1 :: Int)]
