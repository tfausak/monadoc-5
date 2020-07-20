#! /usr/bin/env stack
-- stack exec runghc
module Main where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Environment as Environment

-- | This script replaces the literal string @Nothing@ in a file with the given
-- string. It's like a very limited version of @sed@. It's used to put the Git
-- commit hash into a source file when building on CI. Typical usage looks like
-- this:
--
-- > runghc src/script/set-commit-hash.hs \
-- >   src/lib/Monadoc/Data/Commit.hs \
-- >   $GITHUB_SHA # or ${{ github.sha }}
--
-- For that invocation, let's say @$GITHUB_SHA@ is @01af@. That means the
-- source file will have any instances of @Nothing@ replaced with
-- @(Just "01af")@.
--
-- Why do we do things this way? Originally we used the @githash@ package. That
-- spliced in Git information using Template Haskell. It did so by looking for
-- specific files. Unfortunately Cabal's Nix-style builds don't include Git
-- stuff, so the information was never populated.
--
-- That motivated us to look at CPP solutions. However we quickly ran into two
-- problems: Escaping strings is hard, and passing options is tedious. Trying
-- to get a quoted string through all the layers of build tools was basically
-- impossible. Passing a bare string worked but required escaping with a macro.
-- That would only work if we used @cpphs@ instead of the default. And then on
-- top of all that Cabal makes it very hard to set a GHC option for only one
-- package.
--
-- We briefly considering a TH solution that read an environment variable.
-- Ultimately we decided against that because it wasn't clear how to avoid
-- accidentally caching the wrong information, or causing it to rebuild more
-- frequently than necessary.
--
-- So that's how we ended up here. We chose to write a small Haskell script
-- instead of using @sed@ so that it would work across operating systems.
main :: IO ()
main = do
  [file, hash] <- Environment.getArgs
  contents <- Text.readFile file
  Text.writeFile file $ Text.replace
    (Text.pack "Nothing")
    (Text.pack $ mconcat ["(Just ", show hash, ")"])
    contents
