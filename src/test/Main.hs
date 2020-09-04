import Monadoc.Prelude
import Test.Hspec

import qualified Monadoc.Data.CommitSpec
import qualified Monadoc.Data.MigrationsSpec
import qualified Monadoc.Data.OptionsSpec
import qualified Monadoc.Data.VersionSpec
import qualified Monadoc.Handler.AccountSpec
import qualified Monadoc.Handler.FaviconSpec
import qualified Monadoc.Handler.GitHubCallbackSpec
import qualified Monadoc.Handler.IndexSpec
import qualified Monadoc.Handler.LogoSpec
import qualified Monadoc.Handler.LogOutSpec
import qualified Monadoc.Handler.PackageSpec
import qualified Monadoc.Handler.PingSpec
import qualified Monadoc.Handler.RevisionSpec
import qualified Monadoc.Handler.RobotsSpec
import qualified Monadoc.Handler.SearchSpec
import qualified Monadoc.Handler.TachyonsSpec
import qualified Monadoc.Handler.ThrowSpec
import qualified Monadoc.Handler.VersionSpec
import qualified Monadoc.MainSpec
import qualified Monadoc.Server.ApplicationSpec
import qualified Monadoc.Server.CommonSpec
import qualified Monadoc.Server.MainSpec
import qualified Monadoc.Server.MiddlewareSpec
import qualified Monadoc.Server.RouterSpec
import qualified Monadoc.Server.SettingsSpec
import qualified Monadoc.Server.TemplateSpec
import qualified Monadoc.Type.AppSpec
import qualified Monadoc.Type.BinarySpec
import qualified Monadoc.Type.Cabal.ModuleNameSpec
import qualified Monadoc.Type.Cabal.PackageNameSpec
import qualified Monadoc.Type.Cabal.VersionRangeSpec
import qualified Monadoc.Type.Cabal.VersionSpec
import qualified Monadoc.Type.ConfigResultSpec
import qualified Monadoc.Type.ConfigSpec
import qualified Monadoc.Type.ContextSpec
import qualified Monadoc.Type.EtagSpec
import qualified Monadoc.Type.GitHub.LoginSpec
import qualified Monadoc.Type.GitHub.UserIdSpec
import qualified Monadoc.Type.GitHub.UserSpec
import qualified Monadoc.Type.GuidSpec
import qualified Monadoc.Type.MigrationMismatchSpec
import qualified Monadoc.Type.MigrationSpec
import qualified Monadoc.Type.NotFoundExceptionSpec
import qualified Monadoc.Type.PathSpec
import qualified Monadoc.Type.RevisionSpec
import qualified Monadoc.Type.RouteSpec
import qualified Monadoc.Type.ServiceSpec
import qualified Monadoc.Type.Sha256Spec
import qualified Monadoc.Type.SizeSpec
import qualified Monadoc.Type.TestExceptionSpec
import qualified Monadoc.Type.TimestampSpec
import qualified Monadoc.Type.UrlSpec
import qualified Monadoc.Type.UserSpec
import qualified Monadoc.Type.WithCallStackSpec
import qualified Monadoc.Utility.CabalSpec
import qualified Monadoc.Utility.ConsoleSpec
import qualified Monadoc.Utility.GhcSpec
import qualified Monadoc.Utility.SqlSpec
import qualified Monadoc.Utility.TimeSpec
import qualified Monadoc.Utility.Utf8Spec
import qualified Monadoc.Worker.MainSpec
import qualified MonadocSpec

main :: IO ()
main = hspec <| do
  Monadoc.Data.CommitSpec.spec
  Monadoc.Data.MigrationsSpec.spec
  Monadoc.Data.OptionsSpec.spec
  Monadoc.Data.VersionSpec.spec
  Monadoc.Handler.AccountSpec.spec
  Monadoc.Handler.FaviconSpec.spec
  Monadoc.Handler.GitHubCallbackSpec.spec
  Monadoc.Handler.IndexSpec.spec
  Monadoc.Handler.LogoSpec.spec
  Monadoc.Handler.LogOutSpec.spec
  Monadoc.Handler.PackageSpec.spec
  Monadoc.Handler.PingSpec.spec
  Monadoc.Handler.RevisionSpec.spec
  Monadoc.Handler.RobotsSpec.spec
  Monadoc.Handler.SearchSpec.spec
  Monadoc.Handler.TachyonsSpec.spec
  Monadoc.Handler.ThrowSpec.spec
  Monadoc.Handler.VersionSpec.spec
  Monadoc.MainSpec.spec
  Monadoc.Server.ApplicationSpec.spec
  Monadoc.Server.CommonSpec.spec
  Monadoc.Server.MainSpec.spec
  Monadoc.Server.MiddlewareSpec.spec
  Monadoc.Server.RouterSpec.spec
  Monadoc.Server.SettingsSpec.spec
  Monadoc.Server.TemplateSpec.spec
  Monadoc.Type.AppSpec.spec
  Monadoc.Type.BinarySpec.spec
  Monadoc.Type.Cabal.ModuleNameSpec.spec
  Monadoc.Type.Cabal.PackageNameSpec.spec
  Monadoc.Type.Cabal.VersionRangeSpec.spec
  Monadoc.Type.Cabal.VersionSpec.spec
  Monadoc.Type.ConfigResultSpec.spec
  Monadoc.Type.ConfigSpec.spec
  Monadoc.Type.ContextSpec.spec
  Monadoc.Type.EtagSpec.spec
  Monadoc.Type.GitHub.LoginSpec.spec
  Monadoc.Type.GitHub.UserIdSpec.spec
  Monadoc.Type.GitHub.UserSpec.spec
  Monadoc.Type.GuidSpec.spec
  Monadoc.Type.MigrationMismatchSpec.spec
  Monadoc.Type.MigrationSpec.spec
  Monadoc.Type.NotFoundExceptionSpec.spec
  Monadoc.Type.PathSpec.spec
  Monadoc.Type.RevisionSpec.spec
  Monadoc.Type.RouteSpec.spec
  Monadoc.Type.ServiceSpec.spec
  Monadoc.Type.Sha256Spec.spec
  Monadoc.Type.SizeSpec.spec
  Monadoc.Type.TestExceptionSpec.spec
  Monadoc.Type.TimestampSpec.spec
  Monadoc.Type.UrlSpec.spec
  Monadoc.Type.UserSpec.spec
  Monadoc.Type.WithCallStackSpec.spec
  Monadoc.Utility.CabalSpec.spec
  Monadoc.Utility.ConsoleSpec.spec
  Monadoc.Utility.GhcSpec.spec
  Monadoc.Utility.SqlSpec.spec
  Monadoc.Utility.TimeSpec.spec
  Monadoc.Utility.Utf8Spec.spec
  Monadoc.Worker.MainSpec.spec
  MonadocSpec.spec
