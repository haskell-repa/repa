
module Data.Array.Repa.Vector.Repr.Delayed
        ( D, Array(..)

          -- * Conversions
        , fromFunction
        , toFunction
        , delay

          -- * Checked conversions
        , defaultFromFunction
        , checkedFromFunction)
where
import Data.Array.Repa.Bulk.Par         as Par
import Data.Array.Repa.Bulk.Seq         as Seq
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Compute.Load
import Data.Array.Repa.Vector.Compute.Target
import Data.Array.Repa.Vector.Compute
import Data.Array.Repa.Vector.Operators.Bulk
import GHC.Exts
import Debug.Trace


-- | Delayed arrays are represented as functions from the index to element value.
--
--   Every time you index into a delayed array the element at that position 
--   is recomputed.
data D

-- | Compute elements of a delayed array.
data instance Array D sh a
        = ADelayed  
                !sh 
                (sh -> a) 

instance Bulk D a where
 index       (ADelayed _  f) ix  
        = f ix
 {-# INLINE index #-}

 linearIndex (ADelayed sh f) ix  
        = f (fromIndex sh ix)
 {-# INLINE linearIndex #-}

 extent (ADelayed sh _)
        = sh
 {-# INLINE extent #-}


-- Computation ----------------------------------------------------------------
-- | Compute all elements in an array.
instance Shape sh => Load D sh e where
 loadP (ADelayed sh getElem) mvec
  = mvec `deepSeqMVec` 
    do  traceEventIO "Repa.loadP[Delayed]: start"

        let get   ix  = getElem $ fromIndex sh (I# ix)
        let write ix  = unsafeWriteMVec mvec (I# ix)
        let !(I# len) = size sh
        Par.fillChunked theGang write get len 

        -- The result vector must be live until the computation completes.
        touchMVec mvec

        traceEventIO "Repa.loadP[Delayed]: end"
 {-# INLINE [4] loadP #-}


 loadS (ADelayed sh getElem) mvec
  = mvec `deepSeqMVec` 
    do  traceEventIO "Repa.loadS[Delayed]: start"

        let get   ix  = getElem $ fromIndex sh (I# ix)
        let write ix  = unsafeWriteMVec mvec (I# ix)
        let !(I# len) = size sh

        Seq.fillLinear write get len

        -- The result vector must be live until the computation completes.
        touchMVec mvec

        traceEventIO "Repa.loadS[Delayed]: end"
 {-# INLINE [4] loadS #-}


-- | Compute a range of elements in a rank-2 array.
instance Elt e => LoadRange D DIM2 e where
 loadRangeP  (ADelayed (Z :. _h :. (I# w)) getElem) mvec
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = mvec `deepSeqMVec` 
    do  traceEventIO "Repa.loadRangeP[Delayed]: start"

        let get   y x   = getElem (Z :. (I# y) :. (I# x))
        let write ix    = unsafeWriteMVec mvec (I# ix)

        Par.fillBlock2 theGang write get
                        w x0 y0 w0 h0

        -- The result vector must be live until the computation completes.
        touchMVec mvec

        traceEventIO "Repa.loadRangeP[Delayed]: end"
 {-# INLINE [1] loadRangeP #-}


 loadRangeS  (ADelayed (Z :. _h :. (I# w)) getElem) mvec
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = mvec `deepSeqMVec`
    do  traceEventIO "Repa.loadRangeS[Delayed]: start"

        let get y x     = getElem (Z :. (I# y) :. (I# x))
        let write ix    = unsafeWriteMVec mvec (I# ix)

        Seq.fillBlock2 write get
                w x0 y0 w0 h0

        -- The result vector must be live until the computation completes.
        touchMVec mvec

        traceEventIO "Repa.loadRangeS[Delayed]: end"
 {-# INLINE [1] loadRangeS #-}


-- | Compute a delayed array.
instance Shape sh => Compute D sh a where
 computeIOP arr
  = do  let len = size $ extent arr
        mvec    <- newMVec len
        loadP arr mvec
        unsafeFreezeMVec (extent arr) mvec
 {-# INLINE [4] computeIOP #-}

 computeIOS arr
  = do  let len = size $ extent arr
        mvec    <- newMVec len
        loadP arr mvec
        unsafeFreezeMVec (extent arr) mvec
 {-# INLINE [4] computeIOS #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an element producing function as a delayed array.
--
--   The first argument gives the nominal extent of the array.
--
--   To preserve execution sanity when the result is indexed out-of-bounds,
--   the provided element function must return a deterministic value at any
--   possible index, not just the indices in the nominal extent.
--
--   If you are using `fromFunction` to index into a physical array then
--   consider using either `defaultFromFunction` or `checkedFromFunction`
--   to perform the nessesary bounds checks.
--
fromFunction :: sh -> (sh -> a) -> Array D sh a
fromFunction sh f 
        = ADelayed sh f 
{-# INLINE [4] fromFunction #-}


-- | O(1). Wrap an element producing function as a delayed array, 
--         and perform bounds checks.
--
--   If the resulting array is indexed out of bounds then return
--   the default value instead.
defaultFromFunction
        :: Shape sh
        => a 
        -> sh 
        -> (sh -> a) -> Array D sh a

defaultFromFunction def sh f
 = ADelayed sh get
 where !sz      = size sh
       get ix
        = let !lix      = toIndex sh ix
          in  if lix < 0 || lix >= sz 
                then def
                else f ix
{-# INLINE [4] defaultFromFunction #-}


-- | O(1). Wrap an element producing function as a delayed array, 
--         and perform bounds checks.
--
--   If the resulting array is indexed out of bounds then this will
--   invoke `error` and the provided string will be printed.
--
checkedFromFunction 
        :: (Shape sh, Bulk r a)
        => String -> sh 
        -> (sh -> a) -> Array D sh a

checkedFromFunction msg sh f
 = defaultFromFunction 
        (error $ "checkedFromFunction out of range -- " ++ show msg)
        sh f
{-# INLINE [4] checkedFromFunction #-}


-- | O(1). Produce the extent of an array, and a function to retrieve an
--   arbitrary element.
toFunction 
        :: (Shape sh, Bulk r a)
        => Array r sh a -> (sh, sh -> a)
toFunction arr
 = case delay arr of
        ADelayed sh f -> (sh, f)
{-# INLINE [4] toFunction #-}


-- | O(1). Delay an array.
--   This wraps the internal representation to be a function from
--   indices to elements, so consumers don't need to worry about
--   what the previous representation was.
--
delay   :: (Shape sh, Bulk r a) 
        => Array  r sh a -> Array D sh a
delay arr = ADelayed (extent arr) (index arr)
{-# INLINE [4] delay #-}
