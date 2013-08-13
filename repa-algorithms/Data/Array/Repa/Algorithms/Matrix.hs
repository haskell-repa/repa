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
        , transpose2P, transpose2S

          -- * Trace.
        , trace2P, trace2S)

where
import Data.Array.Repa                  as R
import Data.Array.Repa.Eval             as R
import Data.Array.Repa.Unsafe           as R
import Control.Monad
import Control.Monad.ST.Strict


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
mmultP  :: Monad m
        => Array U DIM2 Double 
        -> Array U DIM2 Double 
        -> m (Array U DIM2 Double)

mmultP arr brr 
 = [arr, brr] `deepSeqArrays` 
   do   trr      <- transpose2P brr
        let (Z :. h1  :. _)  = extent arr
        let (Z :. _   :. w2) = extent brr
        computeP 
         $ fromFunction (Z :. h1 :. w2)
         $ \ix   -> R.sumAllS 
                  $ R.zipWith (*)
                        (unsafeSlice arr (Any :. (row ix) :. All))
                        (unsafeSlice trr (Any :. (col ix) :. All))
{-# NOINLINE mmultP #-}


-- | Matrix matrix multiply, sequentially.
mmultS  :: Array U DIM2 Double 
        -> Array U DIM2 Double 
        -> Array U DIM2 Double

mmultS arr brr
 = [arr, brr]  `deepSeqArrays` runST $
   do   trr     <- R.now $ transpose2S brr
        let (Z :. h1  :. _)  = extent arr
        let (Z :. _   :. w2) = extent brr
        return $ computeS 
         $ fromFunction (Z :. h1 :. w2)
         $ \ix   -> R.sumAllS 
                  $ R.zipWith (*)
                        (unsafeSlice arr (Any :. (row ix) :. All))
                        (unsafeSlice trr (Any :. (col ix) :. All))
{-# NOINLINE mmultS #-}


-- Transpose ------------------------------------------------------------------
-- | Transpose a 2D matrix, in parallel.
transpose2P
        :: Monad m 
        => Array U DIM2 Double 
        -> m (Array U DIM2 Double)

transpose2P arr
 = arr `deepSeqArray`
   do   computeUnboxedP 
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
   do   computeUnboxedS
         $ unsafeBackpermute new_extent swap arr
 where  swap (Z :. i :. j)      = Z :. j :. i
        new_extent              = swap (extent arr)
{-# NOINLINE transpose2S #-}


-- Trace ------------------------------------------------------------------------
-- | Get the trace of a (square) 2D matrix, in parallel.
trace2P :: Monad m => Array U DIM2 Double -> m Double
trace2P x 
 = liftM (safeHead . toList) $ sumP $ slice y (Z :. (0 :: Int) :. All)
 where
    safeHead []     = error "repa-algorithms: trace2P empty list"
    safeHead (x':_) = x'

    y               = unsafeBackpermute (extent x) f x
    f (Z :. i :. j) = Z :. (i - j) `mod` nRows:. j
    Z :. nRows :. _nCols = extent x


-- | Get the trace of a (square) 2D matrix, sequentially.
trace2S :: Array U DIM2 Double -> Double
trace2S x 
 = safeHead $ toList $ sumS $ slice y (Z :. (0 :: Int) :. All)
 where
    safeHead []     = error "repa-algorithms: trace2S empty list"
    safeHead (x':_) = x'

    y               =  unsafeBackpermute (extent x) f x
    f (Z :. i :. j) = Z :. (i - j) `mod` nRows:. j
    Z :. nRows :. _nCols = extent x
