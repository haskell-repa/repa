{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Nice
        ( Nicer (..)
        , Str   (..)
        , Tok   (..))
where
import Data.Int
import Data.Word
import Control.Monad
import Data.Repa.Array.Generic          as A
import Data.Repa.Scalar.Product         as B
import Data.Repa.Scalar.Date32          (Date32)
import Prelude                          as P


-- | Wrapper to indicate a list of characters should be printed as a string,
--   including double quotes.
data Str = Str [Char]

instance Show Str where
 show (Str xs) = show xs


-- | Wrapper to indicate a list of characters should be printed as a string,
--   without double quotes.
data Tok = Tok [Char]

instance Show Tok where
 show (Tok xs) = xs


-- | Convert some value to a nice form.
--
--   In particular:
--
--   * Nested Arrays are converted to nested lists, so that they are easier
--     to work with on the ghci console.
--
--   * Lists of characters are wrapped into the `Str` data type, so that
--     they can be pretty printed differently by follow-on processing.
-- 
--   As ghci automatically pretty prints lists, using @nice@ is more
--   fun than trying to @show@ the raw Repa array representations.
--
class Nicer a where
 type Nice a 
 nice :: a -> Nice a


-- Atomic ---------------------------------------------------------------------
instance Nicer ()  where
 type Nice ()           = ()
 nice x = x

-- Chars
instance Nicer Char where
 type Nice Char         = Char
 nice x = x

-- Floats
instance Nicer Float where
 type Nice Float        = Float
 nice x = x

instance Nicer Double where
 type Nice Double       = Double
 nice x = x

-- Ints
instance Nicer Int where
 type Nice Int          = Int
 nice x = x

instance Nicer Int8 where
 type Nice Int8         = Int8
 nice x = x

instance Nicer Int16 where
 type Nice Int16        = Int16
 nice x = x

instance Nicer Int32 where
 type Nice Int32        = Int32
 nice x = x

instance Nicer Int64 where
 type Nice Int64        = Int64
 nice x = x

-- Words
instance Nicer Word where
 type Nice Word         = Word
 nice x = x

instance Nicer Word8 where
 type Nice Word8        = Word8
 nice x = x

instance Nicer Word16 where
 type Nice Word16       = Word16
 nice x = x

instance Nicer Word32 where
 type Nice Word32       = Word32
 nice x = x

instance Nicer Word64 where
 type Nice Word64       = Word64
 nice x = x

-- Dates
instance Nicer Date32 where
 type Nice Date32       = Date32
 nice x = x



-- Lists ----------------------------------------------------------------------
-- instance (Nicer a) => Nicer [a] where
--  type Nice [a]          = [Nice a]
--  nice xs                = P.map nice xs

-- Special case instance for lists of chars to pretty print them 
-- without the [,] list syntax.
instance Nicer [Char] where
 type Nice [Char]       = Str
 nice xs                = Str xs

instance Nicer [Int] where
 type Nice [Int]        = [Int]
 nice xs                = xs

instance Nicer [Float] where
 type Nice [Float]      = [Float]
 nice xs                = xs

instance Nicer [Double] where
 type Nice [Double]     = [Double]
 nice xs                = xs

instance Nicer [Int8] where
 type Nice [Int8]       = [Int8]
 nice xs                = xs

instance Nicer [Int16] where
 type Nice [Int16]      = [Int16]
 nice xs                = xs

instance Nicer [Int32] where
 type Nice [Int32]      = [Int32]
 nice xs                = xs

instance Nicer [Int64] where
 type Nice [Int64]      = [Int64]
 nice xs                = xs

instance Nicer [Word8] where
 type Nice [Word8]      = [Word8]
 nice xs                = xs

instance Nicer [Word16] where
 type Nice [Word16]     = [Word16]
 nice xs                = xs

instance Nicer [Word32] where
 type Nice [Word32]     = [Word32]
 nice xs                = xs

instance Nicer [Word64] where
 type Nice [Word64]     = [Word64]
 nice xs                = xs


-- Parametric -----------------------------------------------------------------
instance Nicer a 
      => Nicer (Maybe a) where
 type Nice (Maybe a)    = Maybe (Nice a)
 nice x = liftM nice x

instance (Nicer a, Nicer b) 
      => Nicer (a, b) where
 type Nice (a, b)       = (Nice a, Nice b)
 nice (x, y)            = (nice x, nice y)

instance (Nicer a, Nicer b) 
      => Nicer (a :*: b) where
 type Nice (a :*: b)    = (Nice a :*: Nice b)
 nice (x :*: y)         = (nice x :*: nice y)

instance (Bulk l a, Nicer [a]) 
      => Nicer (Array l a) where
 type Nice (Array l a)  = Nice [a]
 nice vec               = nice $ toList vec


instance Nicer a 
      => Nicer [Maybe a] where
 type Nice [Maybe a]    = [Nice (Maybe a)]
 nice xs                = P.map nice xs

instance (Nicer a, Nicer b) 
      => Nicer [(a, b)] where
 type Nice [(a, b)]     = [Nice (a, b)]
 nice xs                = P.map nice xs

instance (Nicer a, Nicer b) 
      => Nicer [(a :*: b)] where
 type Nice [(a :*: b)]  = [Nice (a :*: b)]
 nice xs                = P.map nice xs

instance (Bulk l a, Nicer [a])
      => Nicer [(Array l a)] where
 type Nice [Array l a]  = [Nice [a]]
 nice xs                = P.map (nice . toList) xs

instance Nicer [a]
      => Nicer [[a]] where
 type Nice [[a]]        = [Nice [a]]
 nice xs                = P.map nice xs

