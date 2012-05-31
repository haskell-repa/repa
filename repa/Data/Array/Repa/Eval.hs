{-# LANGUAGE UndecidableInstances #-}

-- | Low level interface to parallel array filling operators.
module Data.Array.Repa.Eval
        ( -- * Element types
          Elt       (..)

        -- * Parallel array filling
        , Target    (..)
        , Load      (..)
        , LoadRange (..)
        , fromList
        
        -- * Converting between representations
        , computeS, computeP, suspendedComputeP
        , copyS,    copyP,    suspendedCopyP
        , now
        
        -- * Chunked filling
        , fillLinearS
        , fillChunkedP
        , fillChunkedIOP

        -- * Interleaved filling
        , fillInterleavedP

        -- * Blockwise filling
        , fillBlock2P
        , fillBlock2S
        
        -- * Cursored blockwise filling
        , fillCursoredBlock2S
        , fillCursoredBlock2P
        
        -- * Chunked selection
        , selectChunkedS
        , selectChunkedP)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Target
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Eval.Chunked
import Data.Array.Repa.Eval.Interleaved
import Data.Array.Repa.Eval.Cursored
import Data.Array.Repa.Eval.Selection
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import System.IO.Unsafe


-- | Parallel computation of array elements.
--
--   * The source array must have a delayed representation like `D`, `C` or `P`, 
--     and the result a manifest representation like `U` or `F`.
--
--   * If you want to copy data between manifest representations then use
--    `copyP` instead.
--
--   * If you want to convert a manifest array back to a delayed representation
--     then use `delay` instead.
--
computeP 
        :: ( Load r1 sh e
           , Target r2 e, Source r2 e, Monad m)
        => Array r1 sh e -> m (Array r2 sh e)
computeP arr = now $ suspendedComputeP arr
{-# INLINE [4] computeP #-}


-- | Sequential computation of array elements.
computeS 
        :: (Load r1 sh e, Target r2 e)
        => Array r1 sh e -> Array r2 sh e
computeS arr1
 = arr1 `deepSeqArray` 
   unsafePerformIO
 $ do   mvec2   <- newMVec (size $ extent arr1) 
        loadS arr1 mvec2
        unsafeFreezeMVec (extent arr1) mvec2
{-# INLINE [4] computeS #-}


-- | Suspended parallel computation of array elements.
--
--   This version creates a thunk that will evaluate the array on demand.
--   If you force it when another parallel computation is already running
--   then you  will get a runtime warning and evaluation will be sequential. 
--   Use `deepSeqArray` and `now` to ensure that each array is evaluated
--   before proceeding to the next one. 
--  
--   If unsure then just use the monadic version `computeP`. This one ensures
--   that each array is fully evaluated before continuing.
--
suspendedComputeP 
        :: (Load r1 sh e, Target r2 e)
        => Array r1 sh e -> Array r2 sh e
suspendedComputeP arr1
 = arr1 `deepSeqArray` 
   unsafePerformIO
 $ do   mvec2    <- newMVec (size $ extent arr1) 
        loadP arr1 mvec2
        unsafeFreezeMVec (extent arr1) mvec2
{-# INLINE [4] suspendedComputeP #-}


-- | Parallel copying of arrays.
--
--   * This is a wrapper that delays an array before calling `computeP`. 
-- 
--   * You can use it to copy manifest arrays between representations.
--
copyP  :: ( Source r1 e, Source r2 e
          , Load D sh e, Target r2 e
          , Monad m)
        => Array r1 sh e -> m (Array r2 sh e)
copyP arr = now $ suspendedCopyP arr
{-# INLINE [4] copyP #-}


-- | Sequential copying of arrays.
copyS   :: ( Source r1 e
           , Load D sh e, Target r2 e)
        => Array r1 sh e -> Array r2 sh e
copyS arr1  = computeS $ delay arr1
{-# INLINE [4] copyS #-}


-- | Suspended parallel copy of array elements.
suspendedCopyP   
        :: ( Source r1 e
           , Load D sh e, Target r2 e)
        => Array r1 sh e -> Array r2 sh e
suspendedCopyP arr1  = suspendedComputeP $ delay arr1
{-# INLINE [4] suspendedCopyP #-}


-- | Monadic version of `deepSeqArray`. 
-- 
--   Forces an suspended array computation to be completed at this point
--   in a monadic computation.
--
-- @ do  let arr2 = suspendedComputeP arr1
--     ...
--     arr3 <- now $ arr2
--     ...
-- @
--
now     :: (Shape sh, Source r e, Monad m)
        => Array r sh e -> m (Array r sh e)
now arr
 = do   arr `deepSeqArray` return ()
        return arr
{-# INLINE [4] now #-}


