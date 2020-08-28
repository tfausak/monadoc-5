module Monadoc.Utility.Ghc where

import qualified Bag
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Function
import qualified Data.Text
import qualified DynFlags
import qualified ErrUtils
import qualified FastString
import qualified GHC
import qualified GHC.Hs
import qualified GHC.LanguageExtensions.Type as X
import qualified GHC.Paths
import qualified HeaderInfo
import qualified Language.Preprocessor.Cpphs as Cpp
import qualified Lexer
import Monadoc.Prelude
import qualified Monadoc.Utility.Utf8 as Utf8
import qualified Outputable
import qualified Parser
import qualified SrcLoc
import qualified StringBuffer

newtype Errors = Errors
  { unwrapErrors :: Bag.Bag ErrUtils.ErrMsg
  }

instance Eq Errors where
  (==) = Data.Function.on (==) show

instance Show Errors where
  show = show <<< Bag.bagToList <<< unwrapErrors

newtype Module = Module
  { unwrapModule :: SrcLoc.Located (GHC.Hs.HsModule GHC.Hs.GhcPs)
  }

instance Eq Module where
  (==) = Data.Function.on (==) show

instance Show Module where
  show = Outputable.showSDocUnsafe <<< Outputable.ppr <<< unwrapModule

parse
  :: [(Bool, X.Extension)]
  -> FilePath
  -> Data.ByteString.ByteString
  -> IO (Either Errors Module)
parse extensions filePath byteString = Control.Exception.handle handler <| do
  dynFlags1 <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags
  let
    dynFlags2 = foldr
      toggleExtension
      (DynFlags.gopt_set dynFlags1 DynFlags.Opt_KeepRawTokenStream)
      extensions
  let text = Utf8.toText byteString
  let string1 = Data.Text.unpack text
  let stringBuffer1 = StringBuffer.stringToStringBuffer string1
  let locatedStrings = HeaderInfo.getOptions dynFlags2 stringBuffer1 filePath
  (dynFlags3, _, _) <- DynFlags.parseDynamicFilePragma dynFlags2 locatedStrings
  string2 <- if DynFlags.xopt X.Cpp dynFlags3
    then Cpp.runCpphs cpphsOptions filePath string1
    else pure string1
  Control.Monad.void <<< Control.Exception.evaluate <| length string2
  let stringBuffer2 = StringBuffer.stringToStringBuffer string2
  let fastString = FastString.mkFastString filePath
  let realSrcLoc = SrcLoc.mkRealSrcLoc fastString 1 1
  let pState1 = Lexer.mkPState dynFlags3 stringBuffer2 realSrcLoc
  pure <| case Lexer.unP Parser.parseModule pState1 of
    Lexer.PFailed pState2 ->
      Left <<< Errors <<< snd <| Lexer.getMessages pState2 dynFlags3
    Lexer.POk pState2 locatedHsModuleGhcPs ->
      let bagErrMsg = snd <| Lexer.getMessages pState2 dynFlags3
      in
        if blank bagErrMsg
          then Right <| Module locatedHsModuleGhcPs
          else Left <| Errors bagErrMsg

cpphsOptions :: Cpp.CpphsOptions
cpphsOptions = Cpp.defaultCpphsOptions
  { Cpp.boolopts = Cpp.defaultBoolOptions { Cpp.warnings = False }
  , Cpp.defines = [] -- TODO
  }

handler :: Control.Exception.SomeException -> IO (Either Errors Module)
handler e = do
  f <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags
  pure
    <<< Left
    <<< Errors
    <<< Bag.unitBag
    <<< ErrUtils.mkPlainErrMsg f SrcLoc.noSrcSpan
    <<< Outputable.text
    <| show e

toggleExtension
  :: (Bool, X.Extension) -> DynFlags.DynFlags -> DynFlags.DynFlags
toggleExtension (enable, extension) =
  if enable then enableExtension extension else disableExtension extension

enableExtension :: X.Extension -> DynFlags.DynFlags -> DynFlags.DynFlags
enableExtension extension oldFlags =
  foldr toggleExtension (DynFlags.xopt_set oldFlags extension)
    <| impliedExtensions extension

disableExtension :: X.Extension -> DynFlags.DynFlags -> DynFlags.DynFlags
disableExtension = flip DynFlags.xopt_unset

-- | <https://github.com/tfausak/monadoc/pull/25#issuecomment-676847301>
impliedExtensions :: X.Extension -> [(Bool, X.Extension)]
impliedExtensions extension = case extension of
  X.AutoDeriveTypeable -> [(True, X.DeriveDataTypeable)]
  X.DeriveTraversable -> [(True, X.DeriveFoldable), (True, X.DeriveFunctor)]
  X.DerivingVia -> [(True, X.DerivingStrategies)]
  X.DuplicateRecordFields -> [(True, X.DisambiguateRecordFields)]
  X.ExistentialQuantification -> [(True, X.ExplicitForAll)]
  X.FlexibleInstances -> [(True, X.TypeSynonymInstances)]
  X.FunctionalDependencies -> [(True, X.MultiParamTypeClasses)]
  X.GADTs -> [(True, X.GADTSyntax), (True, X.MonoLocalBinds)]
  X.ImpredicativeTypes -> [(True, X.RankNTypes)]
  X.JavaScriptFFI -> [(True, X.InterruptibleFFI)]
  X.LiberalTypeSynonyms -> [(True, X.ExplicitForAll)]
  X.MultiParamTypeClasses -> [(True, X.ConstrainedClassMethods)]
  X.ParallelArrays -> [(True, X.ParallelListComp)]
  X.PolyKinds -> [(True, X.KindSignatures)]
  X.QuantifiedConstraints -> [(True, X.ExplicitForAll)]
  X.RankNTypes -> [(True, X.ExplicitForAll)]
  X.RebindableSyntax -> [(False, X.ImplicitPrelude)]
  X.RecordWildCards -> [(True, X.DisambiguateRecordFields)]
  X.ScopedTypeVariables -> [(True, X.ExplicitForAll)]
  X.StandaloneKindSignatures -> [(False, X.CUSKs)]
  X.Strict -> [(True, X.StrictData)]
  X.TemplateHaskell -> [(True, X.TemplateHaskellQuotes)]
  X.TypeFamilies ->
    [ (True, X.ExplicitNamespaces)
    , (True, X.KindSignatures)
    , (True, X.MonoLocalBinds)
    ]
  X.TypeFamilyDependencies -> [(True, X.TypeFamilies)]
  X.TypeInType ->
    [(True, X.DataKinds), (True, X.KindSignatures), (True, X.PolyKinds)]
  X.TypeOperators -> [(True, X.ExplicitNamespaces)]
  _ -> []
