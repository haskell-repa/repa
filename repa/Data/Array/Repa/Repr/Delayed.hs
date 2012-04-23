{-# LANGUAGE MagicHash #-}
module Data.Array.Repa.Repr.Delayed
        ( D, Array(..)
        , fromFunction, toFunction
        , delay)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Cursored
import Data.Array.Repa.Eval.Chunked
import Data.Array.Repa.Eval.Fill
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
data instance Array D sh e
        = ADelayed  
                !sh 
                (sh -> e) 


-- Repr -----------------------------------------------------------------------
-- | Compute elements of a delayed array.
instance Repr D a where
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


-- Fill -----------------------------------------------------------------------
-- | Compute all elements in an array.
instance (Fillable r2 e, Shape sh) => Fill D r2 sh e where
 fillP (ADelayed sh getElem) marr
  = marr `deepSeqMArr` 
    do  traceEventIO "Repa.fillP[Delayed]: start"
        fillChunkedP (size sh) (unsafeWriteMArr marr) (getElem . fromIndex sh) 
        touchMArr marr
        traceEventIO "Repa.fillP[Delayed]: end"
 {-# INLINE [4] fillP #-}

 fillS (ADelayed sh getElem) marr
  = marr `deepSeqMArr` 
    do  traceEventIO "Repa.fillS[Delayed]: start"
        fillChunkedS (size sh) (unsafeWriteMArr marr) (getElem . fromIndex sh)
        touchMArr marr
        traceEventIO "Repa.fillS[Delayed]: end"

 {-# INLINE [4] fillS #-}


-- | Compute a range of elements in a rank-2 array.
instance (Fillable r2 e, Elt e) => FillRange D r2 DIM2 e where
 fillRangeP  (ADelayed (Z :. _h :. (I# w)) getElem) marr
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = marr `deepSeqMArr` 
    do  traceEventIO "Repa.fillRangeP[Delayed]: start"
        fillBlock2P (unsafeWriteMArr marr) 
                        getElem
                        w x0 y0 w0 h0
        touchMArr marr
        traceEventIO "Repa.fillRangeP[Delayed]: end"
 {-# INLINE [1] fillRangeP #-}

 fillRangeS  (ADelayed (Z :. _h :. (I# w)) getElem) marr
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = marr `deepSeqMArr`
    do  traceEventIO "Repa.fillRangeS[Delayed]: start"
        fillBlock2S (unsafeWriteMArr marr) 
                getElem
                w x0 y0 w0 h0
        touchMArr marr
        traceEventIO "Repa.fillRangeS[Delayed]: end"
 {-# INLINE [1] fillRangeS #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a function as a delayed array.
fromFunction :: sh -> (sh -> a) -> Array D sh a
fromFunction sh f 
        = ADelayed sh f 
{-# INLINE fromFunction #-}


-- | O(1). Produce the extent of an array, and a function to retrieve an
--   arbitrary element.
toFunction 
        :: (Shape sh, Repr r1 a)
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
delay   :: (Shape sh, Repr r e)
        => Array r sh e -> Array D sh e
delay arr = ADelayed (extent arr) (unsafeIndex arr)
{-# INLINE delay #-}


