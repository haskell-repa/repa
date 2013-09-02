
-- | Fallback implementations of Series operators.
--
--   Code using these series operators is typically fused and vectorised by
--   the Repa plugin. If this transformation is successful then the resulting
--   GHC Core program will use primitives from the @Data.Array.Repa.Series.Prim@
--   module instead. If the fusion process is not successful then the implementations
--   in this module will be used directly.
--
module Data.Array.Repa.Series.Fallback
        ( -- * Series combinators
          map
        , map2
        , pack

          -- * Process constructors
        , reduce)
where
import Data.Array.Repa.Series.Process
import Data.Vector.Primitive                    (Prim)
import Data.Array.Repa.Series.Series            as S
import Data.Array.Repa.Series.Sel               as S
import Data.Array.Repa.Series.Ref               as Ref
import qualified Data.Vector.Primitive          as P
import Prelude                                  hiding (map)
import GHC.Exts
import System.IO.Unsafe


-- Series combinators ---------------------------------------------------------
-- | Apply a function to all elements of a series.
map     :: forall k a b. (Prim a, Prim b)
        => (a -> b) -> Series k a -> Series k b

map f (S.Series len vec)
 = S.Series len (P.map f vec)
{-# INLINE [0] map #-}


-- | Like `zipWith`, but for equal-length series
map2    :: forall k a b c. (Prim a, Prim b, Prim c)
        => (a -> b -> c) -> Series k a -> Series k b
        -> Series k c

map2 f (S.Series len vec1) (S.Series _len vec2)
 = S.Series len (P.zipWith f vec1 vec2)
{-# INLINE [0] map2 #-}


-- | Pack elements of a series using a selector.
pack    :: forall k1 k2 a. Prim a
        => Sel1 k1 k2 -> Series k1 a -> Series k2 a

pack _ _
 = error "repa-series: Fallback.pack is broken"
{-# NOINLINE pack #-}


-- Process constructors -------------------------------------------------------
-- | Reduce a sequence into an accumulator.
reduce  :: forall k a. Prim a
        => Ref a -> (a -> a -> a) -> a -> Series k a -> Process

reduce ref f z s
 = Process
 $ do   let !x  = foldSeries f z s
        v       <- Ref.read ref
        Ref.write ref (f v x)
{-# INLINE [0] reduce #-}


-- | Combine all elements of a series with an associative operator.
foldSeries
        :: forall k a b. Prim b 
        => (a -> b -> a) -> a -> Series k b -> a

foldSeries f z !source
 = go (int2Word# 0#) z
 where  go !ix !acc
         | geWord# ix (S.length source)
         = acc

         | otherwise
         = let  x = S.index source ix
           in   go (plusWord# ix (int2Word# 1#)) (f acc x)
{-# INLINE [0] foldSeries #-}

