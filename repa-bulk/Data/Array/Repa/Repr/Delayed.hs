
module Data.Array.Repa.Repr.Delayed
        ( D, Array(..)
        , fromFunction, toFunction
        , delay)
where
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Bulk.Load
import Data.Array.Repa.Bulk.Target
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Eval.Elt
import Debug.Trace
import GHC.Exts
import qualified Data.Array.Repa.Eval.Par       as Par
import qualified Data.Array.Repa.Eval.Seq       as Seq


---------------------------------------------------------------------------------------------------
-- | Delayed arrays are represented as functions from the index to element value.
--
--   Every time you index into a delayed array the element at that position 
--   is recomputed.
data D

-- | Compute elements of a delayed array.
instance Shape sh => Bulk D sh a where
 data Array D sh a
        = ADelayed  
                !sh 
                (sh -> a) 

 index       (ADelayed _  f) ix  = f ix
 {-# INLINE index #-}

 linearIndex (ADelayed sh f) ix  = f (fromIndex sh ix)
 {-# INLINE linearIndex #-}

 extent (ADelayed sh _)          = sh
 {-# INLINE extent #-}

 slice  shFrom sh' (ADelayed _sh f)
  = ADelayed sh' f'
  where f' ix = f (addDim shFrom ix )
 {-# INLINE slice #-}


-- Load -------------------------------------------------------------------------------------------
instance Shape sh => Load D sh e where
 loadS (ADelayed sh get) !buf
  = do  let !(I# len)   = size sh
        let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' ix     = get $ fromIndex sh (I# ix)
        Seq.fillLinear  write get' len
        touchBuffer  buf
 {-# INLINE [1] loadS #-}

 loadP gang (ADelayed sh get) !buf
  = do  traceEventIO "Repa.loadP[Delayed]: start"
        let !(I# len)   = size sh
        let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' ix     = get $ fromIndex sh (I# ix)
        Par.fillChunked gang write get' len 
        touchBuffer  buf
        traceEventIO "Repa.loadP[Delayed]: end"
 {-# INLINE [1] loadP #-}


instance Elt e => LoadRange D DIM2 e where
 loadRangeS  (ADelayed (Z :. _h :. (I# w)) get) !buf
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = do  let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' x y    = get (Z :. I# y :. I# x)
        Seq.fillBlock2 write get' w x0 y0 w0 h0
        touchBuffer buf
 {-# INLINE [1] loadRangeS #-}

 loadRangeP  gang
             (ADelayed (Z :. _h :. (I# w)) get) !buf
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = do  traceEventIO "Repa.loadRangeP[Delayed]: start"
        let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' x y    = get (Z :. I# y :. I# x)
        Par.fillBlock2  gang write get' w x0 y0 w0 h0
        touchBuffer  buf
        traceEventIO "Repa.loadRangeP[Delayed]: end"
 {-# INLINE [1] loadRangeP #-}


-- Conversions ------------------------------------------------------------------------------------
-- | O(1). Wrap a function as a delayed array.
fromFunction :: sh -> (sh -> a) -> Array D sh a
fromFunction sh f 
        = ADelayed sh f 
{-# INLINE [1] fromFunction #-}


-- | O(1). Produce the extent of an array, and a function to retrieve an
--   arbitrary element.
toFunction 
        :: Bulk r sh a
        => Array r sh a -> (sh, sh -> a)
toFunction arr
 = case delay arr of
        ADelayed sh f -> (sh, f)
{-# INLINE [1] toFunction #-}


-- | O(1). Delay an array.
--   This wraps the internal representation to be a function from
--   indices to elements, so consumers don't need to worry about
--   what the previous representation was.
--
delay   :: Bulk  r sh e
        => Array r sh e -> Array D sh e
delay arr = ADelayed (extent arr) (index arr)
{-# INLINE [1] delay #-}

