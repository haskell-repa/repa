
module Solver (mmultP) where
import Data.Array.Repa          as R
import Data.Array.Repa.Unsafe  as R


-- | Matrix matrix multiply.
mmultP  :: Monad m
        => Array U DIM2 Double 
        -> Array U DIM2 Double 
        -> m (Array U DIM2 Double)

mmultP arr brr 
 = do   trr      <- transpose2P brr
        let (Z :. h1  :. _)  = extent arr
        let (Z :. _   :. w2) = extent brr
        computeP 
         $ fromFunction (Z :. h1 :. w2)
         $ \ix   -> R.sumAllS 
                  $ R.zipWith (*)
                        (unsafeSlice arr (Any :. (row ix) :. All))
                        (unsafeSlice trr (Any :. (col ix) :. All))
{-# NOINLINE mmultP #-}


-- | Transpose a 2D matrix.
transpose2P
        :: Monad m
        => Array U DIM2 Double 
        -> m (Array U DIM2 Double)

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


