module Monadoc.Ghc ( Errors(..), Module(..), parse ) where

import qualified Bag
import qualified Data.ByteString
import qualified Data.Function
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified DynFlags
import qualified ErrUtils
import qualified FastString
import qualified GHC
import qualified GHC.Hs
import qualified GHC.Paths
import qualified HeaderInfo
import qualified Lexer
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
  show = show . Bag.bagToList . unwrapErrors

newtype Module = Module
  { unwrapModule :: SrcLoc.Located (GHC.Hs.HsModule GHC.Hs.GhcPs)
  }

instance Eq Module where
  (==) = Data.Function.on (==) show

instance Show Module where
  show = Outputable.showSDocUnsafe . Outputable.ppr . unwrapModule

parse
  :: FilePath
  -> Data.ByteString.ByteString
  -> IO (Either Errors Module)
parse filePath byteString = do
  dynFlags1 <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags
  let dynFlags2 = DynFlags.gopt_set dynFlags1 DynFlags.Opt_KeepRawTokenStream
  let text = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode byteString
  let string = Data.Text.unpack text
  let stringBuffer = StringBuffer.stringToStringBuffer string
  let locatedStrings = HeaderInfo.getOptions dynFlags2 stringBuffer filePath
  (dynFlags3, _, _) <- DynFlags.parseDynamicFilePragma dynFlags2 locatedStrings
  let fastString = FastString.mkFastString filePath
  let realSrcLoc = SrcLoc.mkRealSrcLoc fastString 1 1
  let pState1 = Lexer.mkPState dynFlags3 stringBuffer realSrcLoc
  pure $ case Lexer.unP Parser.parseModule pState1 of
    Lexer.PFailed pState2 ->
      Left . Errors . snd $ Lexer.getMessages pState2 dynFlags3
    Lexer.POk pState2 locatedHsModuleGhcPs ->
      let bagErrMsg = snd $ Lexer.getMessages pState2 dynFlags3
      in if null bagErrMsg
        then Right $ Module locatedHsModuleGhcPs
        else Left $ Errors bagErrMsg