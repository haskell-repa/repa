
module Solver (mmultP) where
import Data.Array.Repa  as R


-- | Matrix matrix multiply.
mmultP  :: Array U DIM2 Double 
        -> Array U DIM2 Double 
        -> Array U DIM2 Double

mmultP arr' brr 
 = mmult' arr' (transpose2P brr) 
 where (Z :. h1  :. _)  = extent arr'
       (Z :. _   :. w2) = extent brr

       mmult' arr trr
        = trr `deepSeqArray` computeP
        $ fromFunction (Z :. h1 :. w2)
        $ \ix   -> R.sumAllS 
                 $ R.zipWith (*)
                        (slice arr (Any :. (row ix) :. All))
                        (slice trr (Any :. (col ix) :. All))
{-# INLINE mmultP #-}


-- | Transpose a 2D matrix.
transpose2P
        :: Array U DIM2 Double 
        -> Array U DIM2 Double

transpose2P arr
 = computeUnboxedP
 $ unsafeBackpermute new_extent swap arr
 where  swap (Z :. i :. j)      = Z :. j :. i
        new_extent              = swap (extent arr)
{-# INLINE transpose2P #-}


-- | Take the row number of a rank-2 index.
row :: DIM2 -> Int
row (Z :. r :. _) = r
{-# INLINE row #-}


-- | Take the column number of a rank-2 index.
col :: DIM2 -> Int
col (Z :. _ :. c) = c
{-# INLINE col #-}

