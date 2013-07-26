
-- | Fallback implementations of Series operators.
--
--   Code using these series operators is typically fused and vectorised by
--   the Repa plugin. If this transformation is successful then the resulting
--   GHC Core program will use primitives from the @Data.Array.Repa.Series.Prim@
--   module instead. If the fusion process is not successful then the implementations
--   in this module will be used directly.
--
module Data.Array.Repa.Series.Fallback
        ( map
        , map2
        , fold
        , foldIndex
        , pack)
where
import Data.Array.Repa.Series.Series            as S
import Data.Array.Repa.Series.Sel               as S
import qualified Data.Vector.Unboxed            as U
import Data.Vector.Unboxed                      (Unbox)
import GHC.Exts
import Prelude                                  hiding (map)


-- | Apply a function to all elements of a series.
map     :: forall k a b. (Unbox a, Unbox b)
        => (a -> b) -> Series k a -> Series k b

map f (S.Series len vec)
 = S.Series len (U.map f vec)
{-# INLINE [0] map #-}


-- | Like `zipWith`, but for equal-length series
map2    :: forall k a b c. (Unbox a, Unbox b, Unbox c)
        => (a -> b -> c) -> Series k a -> Series k b
        -> Series k c

map2 f (S.Series len vec1) (S.Series _len vec2)
 = S.Series len (U.zipWith f vec1 vec2)
{-# INLINE [0] map2 #-}


-- | Combine all elements of a series with an associative operator.
fold    :: forall k a b. Unbox b 
        => (a -> b -> a) -> a -> Series k b -> a

fold f z !source
 = go 0# z
 where  go !ix !acc
         | ix >=# S.length source
         = acc

         | otherwise
         = let  x = S.index source ix
           in   go (ix +# 1#) (f acc x)
{-# INLINE [0] fold #-}


-- | Combine all elements of a series with an associative operator.
--   The worker function is given the current index into the series.
foldIndex :: forall k a b. Unbox b 
          => (Int# -> a -> b -> a) -> a -> Series k b -> a

foldIndex f z !source
 = go 0# z
 where  go !ix !acc
         | ix >=# S.length source
         = acc

         | otherwise
         = let  x = S.index source ix
           in   go (ix +# 1#) (f ix acc x)
{-# INLINE [0] foldIndex #-}


-- | Pack elements of a series using a selector.
pack    :: forall k1 k2 a. Unbox a
        => Sel1 k1 k2 -> Series k1 a -> Series k2 a

pack sel1 s
 = let  vec'    = U.map snd
                $ U.filter fst 
                $ U.zip (S.sel1Flags sel1) 
                        (S.seriesVector s)

        !(I# len') = U.length vec'

   in   S.Series len' vec'
{-# INLINE [0] pack #-}

