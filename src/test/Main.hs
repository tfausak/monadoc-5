import qualified Monadoc
import qualified Monadoc.Utility.Console
import qualified Monadoc.Data.Commit
import qualified Monadoc.Data.Migrations
import qualified Monadoc.Data.Options
import qualified Monadoc.Data.Version
import qualified Monadoc.Utility.Ghc
import qualified Monadoc.Handler.Account
import qualified Monadoc.Handler.Favicon
import qualified Monadoc.Handler.GitHubCallback
import qualified Monadoc.Handler.Index
import qualified Monadoc.Handler.Logo
import qualified Monadoc.Handler.LogOut
import qualified Monadoc.Handler.Ping
import qualified Monadoc.Handler.Robots
import qualified Monadoc.Handler.Search
import qualified Monadoc.Handler.Tachyons
import qualified Monadoc.Handler.Throw
import qualified Monadoc.Main
import qualified Monadoc.Server.Application
import qualified Monadoc.Server.Common
import qualified Monadoc.Server.Main
import qualified Monadoc.Server.Middleware
import qualified Monadoc.Server.Router
import qualified Monadoc.Server.Settings
import qualified Monadoc.Server.Template
import qualified Monadoc.Type.App
import qualified Monadoc.Type.Binary
import qualified Monadoc.Type.Cabal.PackageName
import qualified Monadoc.Type.Cabal.VersionRange
import qualified Monadoc.Type.Config
import qualified Monadoc.Type.ConfigResult
import qualified Monadoc.Type.Context
import qualified Monadoc.Type.Etag
import qualified Monadoc.Type.GitHub.Login
import qualified Monadoc.Type.GitHub.User
import qualified Monadoc.Type.GitHub.UserId
import qualified Monadoc.Type.Guid
import qualified Monadoc.Type.Migration
import qualified Monadoc.Type.Exception.MigrationMismatch
import qualified Monadoc.Type.NotFoundException
import qualified Monadoc.Type.Path
import qualified Monadoc.Type.Revision
import qualified Monadoc.Type.Route
import qualified Monadoc.Type.Service
import qualified Monadoc.Type.Sha256
import qualified Monadoc.Type.Size
import qualified Monadoc.Type.TestException
import qualified Monadoc.Type.Timestamp
import qualified Monadoc.Type.Url
import qualified Monadoc.Type.User
import qualified Monadoc.Type.WithCallStack
import qualified Monadoc.Utility.Cabal
import qualified Monadoc.Utility.Sql
import qualified Monadoc.Utility.Utf8
import qualified Monadoc.Worker.Main
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do
  Monadoc.Data.Commit.spec
  Monadoc.Data.Migrations.spec
  Monadoc.Data.Options.spec
  Monadoc.Data.Version.spec
  Monadoc.Handler.Account.spec
  Monadoc.Handler.Favicon.spec
  Monadoc.Handler.GitHubCallback.spec
  Monadoc.Handler.Index.spec
  Monadoc.Handler.Logo.spec
  Monadoc.Handler.LogOut.spec
  Monadoc.Handler.Ping.spec
  Monadoc.Handler.Robots.spec
  Monadoc.Handler.Search.spec
  Monadoc.Handler.Tachyons.spec
  Monadoc.Handler.Throw.spec
  Monadoc.Main.spec
  Monadoc.Server.Application.spec
  Monadoc.Server.Common.spec
  Monadoc.Server.Main.spec
  Monadoc.Server.Middleware.spec
  Monadoc.Server.Router.spec
  Monadoc.Server.Settings.spec
  Monadoc.Server.Template.spec
  Monadoc.spec
  Monadoc.Type.App.spec
  Monadoc.Type.Binary.spec
  Monadoc.Type.Cabal.PackageName.spec
  Monadoc.Type.Cabal.VersionRange.spec
  Monadoc.Type.Config.spec
  Monadoc.Type.ConfigResult.spec
  Monadoc.Type.Context.spec
  Monadoc.Type.Etag.spec
  Monadoc.Type.GitHub.Login.spec
  Monadoc.Type.GitHub.User.spec
  Monadoc.Type.GitHub.UserId.spec
  Monadoc.Type.Guid.spec
  Monadoc.Type.Migration.spec
  Monadoc.Type.Exception.MigrationMismatch.spec
  Monadoc.Type.NotFoundException.spec
  Monadoc.Type.Path.spec
  Monadoc.Type.Revision.spec
  Monadoc.Type.Route.spec
  Monadoc.Type.Service.spec
  Monadoc.Type.Sha256.spec
  Monadoc.Type.Size.spec
  Monadoc.Type.TestException.spec
  Monadoc.Type.Timestamp.spec
  Monadoc.Type.Url.spec
  Monadoc.Type.User.spec
  Monadoc.Type.WithCallStack.spec
  Monadoc.Utility.Cabal.spec
  Monadoc.Utility.Console.spec
  Monadoc.Utility.Ghc.spec
  Monadoc.Utility.Sql.spec
  Monadoc.Utility.Utf8.spec
  Monadoc.Worker.Main.spec
