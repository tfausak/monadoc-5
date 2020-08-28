module Monadoc.Prelude
  ( Control.Applicative.Applicative
  , Control.Applicative.pure
  , Control.Exception.Exception
  , Control.Exception.displayException
  , Control.Monad.Monad
  , Control.Monad.forever
  , Control.Monad.join
  , Control.Monad.unless
  , Control.Monad.when
  , Control.Monad.Fail.MonadFail
  , Control.Monad.Fail.fail
  , Data.Bool.Bool(False, True)
  , Data.Bool.not
  , Data.Bool.otherwise
  , (Data.Bool.&&)
  , (Data.Bool.||)
  , Data.ByteString.ByteString
  , Data.Char.Char
  , Data.Data.Data
  , Data.Either.Either(Left, Right)
  , Data.Either.either
  , Data.Eq.Eq
  , (Data.Eq./=)
  , (Data.Eq.==)
  , Data.Foldable.Foldable
  , Data.Foldable.all
  , Data.Foldable.any
  , Data.Foldable.elem
  , Data.Foldable.fold
  , Data.Foldable.foldl
  , Data.Foldable.foldMap
  , Data.Foldable.foldr
  , Data.Foldable.for_
  , Data.Foldable.length
  , Data.Foldable.notElem
  , Data.Foldable.null
  , Data.Foldable.sequence_
  , Data.Foldable.traverse_
  , Data.Function.flip
  , Data.Functor.Functor
  , Data.Functor.void
  , Data.Int.Int
  , Data.Int.Int8
  , Data.Int.Int16
  , Data.Int.Int32
  , Data.Int.Int64
  , Data.List.break
  , Data.List.cycle
  , Data.List.drop
  , Data.List.dropWhile
  , Data.List.filter
  , Data.List.repeat
  , Data.List.replicate
  , Data.List.reverse
  , Data.List.span
  , Data.List.splitAt
  , Data.List.take
  , Data.List.takeWhile
  , Data.List.unzip
  , Data.List.zip
  , Data.List.zipWith
  , Data.Map.Map
  , Data.Maybe.Maybe(Nothing, Just)
  , Data.Maybe.maybe
  , Data.Monoid.Monoid
  , Data.Monoid.mempty
  , Data.Ord.Ord
  , Data.Ord.Ordering(LT, EQ, GT)
  , Data.Ord.compare
  , Data.Ord.comparing
  , Data.Ord.max
  , Data.Ord.min
  , (Data.Ord.<)
  , (Data.Ord.<=)
  , (Data.Ord.>)
  , (Data.Ord.>=)
  , Data.Ratio.Ratio
  , Data.Ratio.Rational
  , Data.Ratio.denominator
  , Data.Ratio.numerator
  , Data.Semigroup.Semigroup
  , (Data.Semigroup.<>)
  , Data.Set.Set
  , Data.String.String
  , Data.String.lines
  , Data.String.unlines
  , Data.String.unwords
  , Data.String.words
  , Data.Text.Text
  , Data.Traversable.Traversable
  , Data.Traversable.for
  , Data.Traversable.sequence
  , Data.Traversable.traverse
  , Data.Tuple.curry
  , Data.Tuple.fst
  , Data.Tuple.snd
  , Data.Tuple.uncurry
  , Data.Typeable.Proxy(Proxy)
  , Data.Typeable.Typeable
  , Data.Typeable.cast
  , Data.UUID.UUID
  , Data.Word.Word
  , Data.Word.Word8
  , Data.Word.Word16
  , Data.Word.Word32
  , Data.Word.Word64
  , GHC.Enum.Bounded
  , GHC.Enum.Enum
  , GHC.Enum.fromEnum
  , GHC.Enum.maxBound
  , GHC.Enum.minBound
  , GHC.Err.error
  , GHC.Err.undefined
  , GHC.Float.Float
  , GHC.Float.Floating
  , GHC.Float.isInfinite
  , GHC.Float.isNaN
  , GHC.Float.logBase
  , GHC.Float.sqrt
  , (GHC.Float.**)
  , GHC.Float.Double
  , GHC.Generics.Generic
  , GHC.Integer.Integer
  , GHC.Num.Num
  , GHC.Num.abs
  , GHC.Num.fromInteger
  , GHC.Num.negate
  , (GHC.Num.+)
  , (GHC.Num.-)
  , GHC.Prim.seq
  , GHC.Real.Fractional
  , GHC.Real.Integral
  , GHC.Real.RealFrac
  , GHC.Real.ceiling
  , GHC.Real.div
  , GHC.Real.divMod
  , GHC.Real.even
  , GHC.Real.floor
  , GHC.Real.fromIntegral
  , GHC.Real.fromRational
  , GHC.Real.mod
  , GHC.Real.odd
  , GHC.Real.quot
  , GHC.Real.quotRem
  , GHC.Real.realToFrac
  , GHC.Real.rem
  , GHC.Real.round
  , GHC.Real.toInteger
  , GHC.Real.toRational
  , GHC.Real.truncate
  , (GHC.Real./)
  , (GHC.Real.^)
  , (GHC.Real.^^)
  , Numeric.Natural.Natural
  , System.IO.FilePath
  , System.IO.IO
  , System.IO.appendFile
  , System.IO.getChar
  , System.IO.getContents
  , System.IO.getLine
  , System.IO.interact
  , System.IO.print
  , System.IO.putChar
  , System.IO.putStr
  , System.IO.putStrLn
  , System.IO.readFile
  , System.IO.writeFile
  , System.IO.Error.IOError
  , System.IO.Error.userError
  , Text.Read.Read
  , Text.Show.Show
  , Text.Show.show
  , always
  , identity
  , lookup
  , map
  , notNull
  , read
  , toEnum
  , (*)
  , (<<<)
  , (<|)
  , (>>>)
  , (|>)
  )
where

import qualified Control.Applicative
import qualified Control.Category
import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.Fail
import qualified Data.Bool
import qualified Data.ByteString
import qualified Data.Char
import qualified Data.Data
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Int
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Ratio
import qualified Data.Semigroup
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Data.Traversable
import qualified Data.Tuple
import qualified Data.Typeable
import qualified Data.UUID
import qualified Data.Word
import qualified GHC.Enum
import qualified GHC.Err
import qualified GHC.Float
import qualified GHC.Generics
import qualified GHC.Integer
import qualified GHC.Num
import qualified GHC.Prim
import qualified GHC.Real
import qualified Numeric.Natural
import qualified System.IO
import qualified System.IO.Error
import qualified Text.Read
import qualified Text.Show

always :: a -> b -> a
always = Data.Function.const

identity :: a -> a
identity = Data.Function.id

lookup
  :: (Data.Foldable.Foldable t, Data.Eq.Eq k)
  => k
  -> t (k, v)
  -> Data.Maybe.Maybe v
lookup k xs = Data.List.lookup k (Data.Foldable.toList xs)

map :: Data.Functor.Functor f => (a -> b) -> f a -> f b
map = Data.Functor.fmap

notNull :: Data.Foldable.Foldable t => t a -> Data.Bool.Bool
notNull = Data.Bool.not Data.Function.. Data.Foldable.null

read :: Text.Read.Read a => Data.String.String -> Data.Maybe.Maybe a
read = Text.Read.readMaybe

toEnum
  :: forall a
   . (GHC.Enum.Bounded a, GHC.Enum.Enum a)
  => Data.Int.Int
  -> Data.Maybe.Maybe a
toEnum n =
  let
    tooSmall = n Data.Ord.< GHC.Enum.fromEnum @a GHC.Enum.minBound
    tooLarge = n Data.Ord.> GHC.Enum.fromEnum @a GHC.Enum.maxBound
  in if tooSmall Data.Bool.|| tooLarge
    then Data.Maybe.Nothing
    else Data.Maybe.Just (GHC.Enum.toEnum n)

-- Redefined here to avoid a stylish-haskell bug.
(*) :: GHC.Num.Num a => a -> a -> a
(*) = (GHC.Num.*)
infixl 7 *

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) = (Control.Category.<<<)
infixr 9 <<<

(<|) :: (a -> b) -> a -> b
(<|) = (Data.Function.$)
infixr 0 <|

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = (Control.Category.>>>)
infixl 9 >>>

(|>) :: a -> (a -> b) -> b
(|>) = (Data.Function.&)
infixl 0 |>
