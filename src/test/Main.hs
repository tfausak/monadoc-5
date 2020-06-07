module Main
  ( main
  )
where

import qualified Test.Hspec as Hspec

import qualified Monadoc.CabalSpec
import qualified Monadoc.ConsoleSpec
import qualified Monadoc.Data.CommitSpec
import qualified Monadoc.Data.MigrationsSpec
import qualified Monadoc.Data.OptionsSpec
import qualified Monadoc.Data.VersionSpec
import qualified Monadoc.GhcSpec
import qualified Monadoc.MainSpec
import qualified Monadoc.Server.MainSpec
import qualified Monadoc.Server.SettingsSpec
import qualified Monadoc.Type.AppSpec
import qualified Monadoc.Type.BinarySpec
import qualified Monadoc.Type.ConfigSpec
import qualified Monadoc.Type.ContextSpec
import qualified Monadoc.Type.EtagSpec
import qualified Monadoc.Type.HandlerSpec
import qualified Monadoc.Type.MigrationMismatchSpec
import qualified Monadoc.Type.MigrationSpec
import qualified Monadoc.Type.Sha256Spec
import qualified Monadoc.Type.SizeSpec
import qualified Monadoc.Type.TimestampSpec
import qualified Monadoc.Type.UrlSpec
import qualified Monadoc.Type.WithCallStackSpec
import qualified Monadoc.Utility.Utf8Spec
import qualified Monadoc.Vendor.SqlSpec
import qualified Monadoc.Vendor.TimeSpec
import qualified Monadoc.Worker.MainSpec
import qualified MonadocSpec

main :: IO ()
main = Hspec.hspec $ do
  Monadoc.CabalSpec.spec
  Monadoc.ConsoleSpec.spec
  Monadoc.Data.CommitSpec.spec
  Monadoc.Data.MigrationsSpec.spec
  Monadoc.Data.OptionsSpec.spec
  Monadoc.Data.VersionSpec.spec
  Monadoc.GhcSpec.spec
  Monadoc.MainSpec.spec
  Monadoc.Server.MainSpec.spec
  Monadoc.Server.SettingsSpec.spec
  Monadoc.Type.AppSpec.spec
  Monadoc.Type.BinarySpec.spec
  Monadoc.Type.ConfigSpec.spec
  Monadoc.Type.ContextSpec.spec
  Monadoc.Type.EtagSpec.spec
  Monadoc.Type.HandlerSpec.spec
  Monadoc.Type.MigrationMismatchSpec.spec
  Monadoc.Type.MigrationSpec.spec
  Monadoc.Type.Sha256Spec.spec
  Monadoc.Type.SizeSpec.spec
  Monadoc.Type.TimestampSpec.spec
  Monadoc.Type.UrlSpec.spec
  Monadoc.Type.WithCallStackSpec.spec
  Monadoc.Utility.Utf8Spec.spec
  Monadoc.Vendor.SqlSpec.spec
  Monadoc.Vendor.TimeSpec.spec
  Monadoc.Worker.MainSpec.spec
  MonadocSpec.spec
