
module Data.Repa.Array.Internals.Operator.Reduce
        ( foldl

          -- | Specialised reductions.
        , prod, sum
        , mean, std)
where
import Data.Repa.Array.Index                            as A
import Data.Repa.Array.Delayed                          as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Eval.Stream                            as A
import qualified Data.Vector.Fusion.Stream              as S
import Prelude 
        as P hiding (foldl, sum)
#include "repa-array.h"


-- | Left fold of all elements in an array, sequentially.
foldl  :: (Bulk l b, Index l ~ Int)
        => (a -> b -> a) -> a -> Array l b -> a

foldl f z arr
        = S.foldl f z 
        $ streamOfArray arr
{-# INLINE_ARRAY foldl #-}


-- | Yield the sum of the elements of an array.
sum    :: (BulkI l a, Num a) => Array l a -> a
sum arr = foldl (+) 0 arr
{-# INLINE sum #-}


-- | Yield the product of the elements of an array.
prod   :: (BulkI l a, Num a) => Array l a -> a
prod arr = foldl (*) 1 arr
{-# INLINE prod #-}


-- | Yield the mean value of the elements of an array.
mean   :: (BulkI l a, Fractional a) 
        => Array l a -> a
mean arr = sum arr / fromIntegral (A.length arr)
{-# INLINE mean #-}


-- | Yield the standard deviation of the elements of an array
std    :: (BulkI l a, Floating a)    
        => Array l a -> a
std arr  
 = let  !u      = mean arr
   in   sqrt $ mean $ A.map (\x -> (x - u) ^ (2 :: Int)) arr
{-# INLINE std #-}
