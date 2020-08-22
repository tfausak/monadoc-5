module Prelude
  ( Data.Bool.Bool(False, True)
  , Data.Either.Either(Left, Right)
  , Data.Int.Int
  , Data.Maybe.Maybe(Nothing, Just)
  , Data.String.String
  , Data.Word.Word
  , GHC.Float.Double
  , GHC.Integer.Integer
  , System.IO.FilePath
  , System.IO.IO
  , Control.Applicative.Applicative
  , Control.Monad.Monad
  , Control.Monad.MonadFail
  , Data.Eq.Eq
  , Data.Functor.Functor
  , Data.Monoid.Monoid
  , Data.Ord.Ord
  , Data.Semigroup.Semigroup
  , Text.Read.Read
  , Text.Show.Show
  , Control.Applicative.pure
  , Control.Monad.fail
  , Control.Monad.mapM
  , Control.Monad.mapM_
  , Control.Monad.return
  , Data.Bool.not
  , Data.Bool.otherwise
  , Data.Either.either
  , Data.Foldable.foldr
  , Data.Foldable.length
  , Data.Foldable.null
  , Data.Function.const
  , Data.Function.flip
  , Data.Function.id
  , Data.Functor.fmap
  , Data.List.concatMap
  , Data.List.drop
  , Data.List.elem
  , Data.List.filter
  , Data.List.lines
  , Data.List.lookup
  , Data.List.replicate
  , Data.List.reverse
  , Data.List.splitAt
  , Data.List.take
  , Data.List.unlines
  , Data.List.unwords
  , Data.List.words
  , Data.Maybe.maybe
  , Data.Monoid.mappend
  , Data.Monoid.mconcat
  , Data.Monoid.mempty
  , Data.Ord.compare
  , Data.Ord.max
  , Data.Ord.min
  , Data.Tuple.curry
  , Data.Tuple.fst
  , Data.Tuple.snd
  , Data.Tuple.uncurry
  , GHC.Real.div
  , GHC.Real.divMod
  , GHC.Real.fromIntegral
  , GHC.Real.mod
  , GHC.Real.quot
  , GHC.Real.quotRem
  , GHC.Real.rem
  , GHC.Real.round
  , System.IO.Error.userError
  , System.IO.print
  , System.IO.putStr
  , System.IO.putStrLn
  , Text.Read.read
  , Text.Show.show
  , (Data.Bool.&&)
  , (Data.Bool.||)
  , (Data.Eq./=)
  , (Data.Eq.==)
  , (Data.Function.$)
  , (Data.Function..)
  , (Data.List.++)
  , (Data.Ord.<)
  , (Data.Ord.<=)
  , (Data.Ord.>)
  , (Data.Ord.>=)
  , (Data.Semigroup.<>)
  , (GHC.Num.*)
  , (GHC.Num.+)
  , (GHC.Num.-)
  , (GHC.Real./)
  )
where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Bool
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Int
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Semigroup
import qualified Data.String
import qualified Data.Tuple
import qualified Data.Word
import qualified GHC.Float
import qualified GHC.Integer
import qualified GHC.Num
import qualified GHC.Real
import qualified System.IO
import qualified System.IO.Error
import qualified Text.Read
import qualified Text.Show
