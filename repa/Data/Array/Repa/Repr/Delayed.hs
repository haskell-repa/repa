{-# LANGUAGE MagicHash #-}
module Data.Array.Repa.Repr.Delayed
        ( D, Array(..)
        , fromFunction, toFunction
        , delay)
where
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Eval.Target
import Data.Array.Repa.Eval.Chunked
import Data.Array.Repa.Eval.Cursored
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Debug.Trace
import GHC.Exts

-- | Delayed arrays are represented as functions from the index to element value.
--
--   Every time you index into a delayed array the element at that position 
--   is recomputed.
data D

-- | Compute elements of a delayed array.
instance Source D a where
 data Array D sh a
        = ADelayed  
                !sh 
                (sh -> a) 

 index       (ADelayed _  f) ix  = f ix
 {-# INLINE index #-}

 linearIndex (ADelayed sh f) ix  = f (fromIndex sh ix)
 {-# INLINE linearIndex #-}

 extent (ADelayed sh _)
        = sh
 {-# INLINE extent #-}

 deepSeqArray (ADelayed sh f) y
        = sh `deepSeq` f `seq` y
 {-# INLINE deepSeqArray #-}


-- Load -----------------------------------------------------------------------
-- | Compute all elements in an array.
instance Shape sh => Load D sh e where
 loadP (ADelayed sh getElem) mvec
  = mvec `deepSeqMVec` 
    do  traceEventIO "Repa.loadP[Delayed]: start"
        fillChunkedP (size sh) (unsafeWriteMVec mvec) (getElem . fromIndex sh) 
        touchMVec mvec
        traceEventIO "Repa.loadP[Delayed]: end"
 {-# INLINE [4] loadP #-}

 loadS (ADelayed sh getElem) mvec
  = mvec `deepSeqMVec` 
    do  traceEventIO "Repa.loadS[Delayed]: start"
        fillLinearS (size sh) (unsafeWriteMVec mvec) (getElem . fromIndex sh)
        touchMVec mvec
        traceEventIO "Repa.loadS[Delayed]: end"
 {-# INLINE [4] loadS #-}


-- | Compute a range of elements in a rank-2 array.
instance Elt e => LoadRange D DIM2 e where
 loadRangeP  (ADelayed (Z :. _h :. (I# w)) getElem) mvec
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = mvec `deepSeqMVec` 
    do  traceEventIO "Repa.loadRangeP[Delayed]: start"
        fillBlock2P (unsafeWriteMVec mvec) 
                        getElem
                        w x0 y0 w0 h0
        touchMVec mvec
        traceEventIO "Repa.loadRangeP[Delayed]: end"
 {-# INLINE [1] loadRangeP #-}

 loadRangeS  (ADelayed (Z :. _h :. (I# w)) getElem) mvec
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = mvec `deepSeqMVec`
    do  traceEventIO "Repa.loadRangeS[Delayed]: start"
        fillBlock2S (unsafeWriteMVec mvec) 
                getElem
                w x0 y0 w0 h0
        touchMVec mvec
        traceEventIO "Repa.loadRangeS[Delayed]: end"
 {-# INLINE [1] loadRangeS #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a function as a delayed array.
fromFunction :: sh -> (sh -> a) -> Array D sh a
fromFunction sh f 
        = ADelayed sh f 
{-# INLINE fromFunction #-}


-- | O(1). Produce the extent of an array, and a function to retrieve an
--   arbitrary element.
toFunction 
        :: (Shape sh, Source r1 a)
        => Array r1 sh a -> (sh, sh -> a)
toFunction arr
 = case delay arr of
        ADelayed sh f -> (sh, f)
{-# INLINE toFunction #-}


-- | O(1). Delay an array.
--   This wraps the internal representation to be a function from
--   indices to elements, so consumers don't need to worry about
--   what the previous representation was.
--
delay   :: Shape sh => Source r e
        => Array r sh e -> Array D sh e
delay arr = ADelayed (extent arr) (unsafeIndex arr)
{-# INLINE delay #-}


