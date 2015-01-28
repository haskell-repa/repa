{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Delayed
        ( D(..), Array(..)
        , fromFunction, toFunction
        , delay
        , map)
--        , zipWith
--        , (+^), (-^), (*^), (/^))
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Load
import Data.Repa.Array.Internals.Target
-- import Data.Array.Repa.Eval.Elt
import Debug.Trace
import GHC.Exts
import qualified Data.Array.Repa.Eval.Par       as Par
import qualified Data.Array.Repa.Eval.Seq       as Seq
import Prelude hiding (map, zipWith)
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Delayed arrays are represented as functions from the index to element
--   value.
--
--   Every time you index into a delayed array the element at that position 
--   is recomputed.
data D l = D l


instance Layout l
      => Layout (D l) where

      type Index (D l)    = Index l
      extent     (D l)    = extent l
      toIndex    (D l) ix = toIndex l ix
      fromIndex  (D l) i  = fromIndex l i
      {-# INLINE extent    #-}
      {-# INLINE toIndex   #-}
      {-# INLINE fromIndex #-}


-- | Delayed arrays.
instance Layout l => Bulk (D l) a where
 data Array (D l) a
        = ADelayed !l (Index l -> a) 

 layout (ADelayed l _)      = D l
 {-# INLINE_ARRAY layout #-}

 index  (ADelayed _l f) ix  = f ix
 {-# INLINE_ARRAY index #-}



-- Load -----------------------------------------------------------------------
instance (Layout l1, Target l2 a)
      =>  Load (D l1) l2 a where
 loadS (ADelayed l1 get) !buf
  = do  let !(I# len)   = size (extent l1)
        let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' ix     = get $ fromIndex   l1  (I# ix)
        Seq.fillLinear  write get' len
        touchBuffer  buf
 {-# INLINE_ARRAY loadS #-}

 loadP gang (ADelayed l1 get) !buf
  = do  traceEventIO "Repa.loadP[Delayed]: start"
        let !(I# len)   = size (extent l1)
        let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' ix     = get $ fromIndex   l1  (I# ix)
        Par.fillChunked gang write get' len 
        touchBuffer  buf
        traceEventIO "Repa.loadP[Delayed]: end"
 {-# INLINE_ARRAY loadP #-}

{-
instance Elt e => LoadRange D DIM2 e where
 loadRangeS  (ADelayed (Z :. _h :. (I# w)) get) !buf
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = do  let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' x y    = get (Z :. I# y :. I# x)
        Seq.fillBlock2 write get' w x0 y0 w0 h0
        touchBuffer buf
 {-# INLINE_ARRAY loadRangeS #-}

 loadRangeP  gang
             (ADelayed (Z :. _h :. (I# w)) get) !buf
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = do  traceEventIO "Repa.loadRangeP[Delayed]: start"
        let write ix x  = unsafeWriteBuffer buf (I# ix) x
        let get' x y    = get (Z :. I# y :. I# x)
        Par.fillBlock2  gang write get' w x0 y0 w0 h0
        touchBuffer  buf
        traceEventIO "Repa.loadRangeP[Delayed]: end"
 {-# INLINE_ARRAY loadRangeP #-}
-}

-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a function as a delayed array.
fromFunction :: l -> (Index l -> a) -> Array (D l) a
fromFunction l f 
        = ADelayed l f 
{-# INLINE_ARRAY fromFunction #-}


-- | O(1). Produce the extent of an array, and a function to retrieve an
--   arbitrary element.
toFunction  :: Bulk  l a
            => Array (D l) a -> (l, Index l -> a)
toFunction (ADelayed l f) = (l, f)
{-# INLINE_ARRAY toFunction #-}


-- | O(1). Delay an array.
--   This wraps the internal representation to be a function from
--   indices to elements, so consumers don't need to worry about
--   what the previous representation was.
--
delay   :: Bulk  l a
        => Array l a -> Array (D l) a
delay arr = ADelayed (layout arr) (index arr)
{-# INLINE_ARRAY delay #-}


-- Operators ------------------------------------------------------------------
-- | Apply a worker function to each element of an array, 
--   yielding a new array with the same extent.
map     :: (Bulk l a, Index (Index l) ~ Index l)
        => (a -> b) -> Array l a -> Array (D (Index l)) b
map f arr
        = ADelayed (extent $ layout arr) 
                   (f . index arr)
{-# INLINE_ARRAY map #-}


-- ZipWith --------------------------------------------------------------------
{-
-- | Combine two arrays, element-wise, with a binary operator.
--      If the extent of the two array arguments differ,
--      then the resulting array's extent is their intersection.
zipWith :: (Bulk l1 a, Bulk l2 b, Index l1 ~ Index l2)
        => (a -> b -> c)
        -> Array l1 a -> Array l2 b
        -> Array (D (Index l1))  sh c

zipWith f arr1 arr2
 = fromFunction (intersectDim (extent arr1) (extent arr2)) 
                get_zipWith
 where  get_zipWith ix  
         = f (arr1 `index` ix) (arr2 `index` ix)
        {-# INLINE get_zipWith #-}
{-# INLINE_ARRAY zipWith #-}
-}
{-
infixl 7  *^, /^
infixl 6  +^, -^

(+^)    = zipWith (+)
{-# INLINE (+^) #-}

(-^)    = zipWith (-)
{-# INLINE (-^) #-}

(*^)    = zipWith (*)
{-# INLINE (*^) #-}

(/^)    = zipWith (/)
{-# INLINE (/^) #-}
-}
