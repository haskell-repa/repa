
-- | Reduction operations on arrays.
module Data.Repa.Array.Internals.Operator.Reduce
        ( foldl

          -- | Specialised reductions.
        , product, sum
        , mean, std
        , correlate)
where
import Data.Repa.Array.Generic.Index                    as A
import Data.Repa.Array.Meta.Delayed                     as A
import Data.Repa.Array.Meta.Tuple                       as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Eval.Stream                            as A
import qualified Data.Vector.Fusion.Stream              as S
import Prelude 
        as P hiding (foldl, sum, product)
#include "repa-array.h"


-- | Left fold of all elements in an array, sequentially.
foldl  :: (Bulk l b, Index l ~ Int)
        => (a -> b -> a) -> a -> Array l b -> a

foldl f z arr
        = S.foldl' f z 
        $ streamOfArray arr
{-# INLINE_ARRAY foldl #-}


-- | Yield the sum of the elements of an array.
sum    :: (BulkI l a, Num a) => Array l a -> a
sum arr = foldl (+) 0 arr
{-# INLINE sum #-}


-- | Yield the product of the elements of an array.
product   :: (BulkI l a, Num a) => Array l a -> a
product arr = foldl (*) 1 arr
{-# INLINE product #-}


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


-- | Compute the Pearson correlation of two arrays.
--
--   If the arrays differ in length then only the common
--   prefix is correlated.
--
correlate 
        :: ( BulkI l1 a, BulkI l2 a
           , Floating a)
        => Array l1 a
        -> Array l2 a
        -> a

correlate arr1 arr2
 = let
        (sX, sX2, sY, sY2, sXY)
         = foldl (\  ( sXa, sX2a, sYa, sY2a, sXYa) (x, y)
                  -> ( sXa + x, sX2a + (x * x)
                     , sYa + y, sY2a + (y * y)
                              , sXYa + (x * y)))
                 (0, 0, 0, 0, 0)
         $ tup2 arr1 arr2

        !n     = min (A.length arr1) (A.length arr2)
        !n'    = fromIntegral n

        !corr  = (/) ((n' * sXY) - (sX * sY)) 
                     ((*) (sqrt ((n' * sX2) - (sX * sX)))
                          (sqrt ((n' * sY2) - (sY * sY))))

  in    corr
{-# INLINE correlate #-}

