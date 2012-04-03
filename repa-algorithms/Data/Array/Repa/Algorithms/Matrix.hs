{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE PackageImports #-}

-- | Algorithms operating on matrices.
-- 
--   These functions should give performance comparable with nested loop C
--   implementations. 
-- 
--   If you care deeply about runtime performance then you
--   may be better off using a binding to LAPACK, such as hvector.
--
module Data.Array.Repa.Algorithms.Matrix
        ( --  * Projections
          row
        , col

          -- * Matrix Multiplication.
	, mmultP,      mmultS

          -- * Transposition.
        , transpose2P, transpose2S) 
where
import Data.Array.Repa                  as R


-- Projections ----------------------------------------------------------------
-- | Take the row number of a rank-2 index.
row :: DIM2 -> Int
row (Z :. r :. _) = r
{-# INLINE row #-}


-- | Take the column number of a rank-2 index.
col :: DIM2 -> Int
col (Z :. _ :. c) = c
{-# INLINE col #-}


-- MMult ----------------------------------------------------------------------
-- | Matrix matrix multiply, in parallel.
mmultP  :: Array U DIM2 Double 
        -> Array U DIM2 Double 
        -> Array U DIM2 Double

mmultP arr' brr 
 = mmult' arr' (transpose2P brr) 
 where mmult' arr trr
        = trr `deepSeqArray` computeP
        $ fromFunction (extent arr)
        $ \ix   -> R.sumAllS 
                 $ R.zipWith (*)
                        (slice arr (Any :. (row ix) :. All))
                        (slice trr (Any :. (col ix) :. All))
{-# NOINLINE mmultP #-}


-- | Matrix matrix multiply, sequentially.
mmultS  :: Array U DIM2 Double 
        -> Array U DIM2 Double 
        -> Array U DIM2 Double

mmultS arr' brr 
 = mmult' arr' (transpose2S brr) 
 where mmult' arr trr
        = trr `deepSeqArray` computeS
        $ fromFunction (extent arr)
        $ \ix   -> R.sumAllS 
                 $ R.zipWith (*)
                        (slice arr (Any :. (row ix) :. All))
                        (slice trr (Any :. (col ix) :. All))
{-# NOINLINE mmultS #-}


-- Transpose ------------------------------------------------------------------
-- | Transpose a 2D matrix, in parallel.
transpose2P
        :: Array U DIM2 Double 
        -> Array U DIM2 Double

transpose2P arr
 = arr `deepSeqArray`
   computeUnboxedP
 $ unsafeBackpermute new_extent swap arr
 where  swap (Z :. i :. j)      = Z :. j :. i
        new_extent              = swap (extent arr)
{-# NOINLINE transpose2P #-}


-- | Transpose a 2D matrix, sequentially.
transpose2S
        :: Array U DIM2 Double 
        -> Array U DIM2 Double

transpose2S arr
 = arr `deepSeqArray`
   computeUnboxedS
 $ unsafeBackpermute new_extent swap arr
 where  swap (Z :. i :. j)      = Z :. j :. i
        new_extent              = swap (extent arr)
{-# NOINLINE transpose2S #-}

