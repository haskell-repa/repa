
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
        , fill

          -- * Process constructors
        , reduce)
where
import Data.Array.Repa.Series.Process
import Data.Array.Repa.Series.Vector
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

map !_ !_
 = unsafePerformIO
 $ do   putStrLn "map!"
        return (error "repa-series: fallback map is broken")
{-# NOINLINE map #-}


-- | Like `zipWith`, but for equal-length series
map2    :: forall k a b c. (Prim a, Prim b, Prim c)
        => (a -> b -> c) -> Series k a -> Series k b
        -> Series k c

map2 !_ !_ !_
 = unsafePerformIO
 $ do   putStrLn "map2!"
        return (error "repa-series: fallback map2 is broken")
{-# NOINLINE map2 #-}


-- | Pack elements of a series using a selector.
pack    :: forall k1 k2 a. Prim a
        => Sel1 k1 k2 -> Series k1 a -> Series k2 a

pack !_ !_
 = unsafePerformIO
 $ do   putStrLn "pack!"
        return (error "repa-series: fallback pack is broken")
{-# NOINLINE pack #-}


-- Process constructors -------------------------------------------------------
-- | Fill a vector buffer with elements of a series.
fill    :: forall k a. Prim a
        => Vector a -> Series k a -> Process

fill !_ !_
 = Process
 $ do   putStrLn "fill!"
        return ()
{-# NOINLINE fill #-}


-- | Reduce a sequence into an accumulator.
reduce  :: forall k a. Prim a
        => Ref a -> (a -> a -> a) -> a -> Series k a -> Process

reduce !ref !f !z !s
 = Process
 $ do   putStrLn "reduce!"
        let !x  = foldSeries f z s
        v       <- Ref.read ref
        Ref.write ref (f v x)
{-# NOINLINE reduce #-}


-- | Combine all elements of a series with an associative operator.
foldSeries
        :: forall k a b. Prim b 
        => (a -> b -> a) -> a -> Series k b -> a

foldSeries f z !source
 = go (int2Word# 0#) z
 where  go !ix !acc
         | 1# <- geWord# ix (S.length source)
         = acc

         | otherwise
         = let  x = S.index source ix
           in   go (plusWord# ix (int2Word# 1#)) (f acc x)
{-# NOINLINE foldSeries #-}

