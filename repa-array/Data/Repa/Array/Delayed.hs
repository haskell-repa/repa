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
import Debug.Trace
import GHC.Exts
import qualified Data.Array.Repa.Eval.Par       as Par
import qualified Data.Array.Repa.Eval.Seq       as Seq
import Prelude hiding (map, zipWith)
#include "repa-array.h"


-------------------------------------------------------------------------------
-- | Delayed arrays wrap functions from an index to element value.
--   The index space is specified by an inner layout, @l@.
--
--   Every time you index into a delayed array the element at that position 
--   is recomputed.
data D l 
        = Delayed 
        { delayedLayout :: l }

deriving instance Eq   l => Eq   (D l)
deriving instance Show l => Show (D l)


-------------------------------------------------------------------------------
-- | Delayed arrays.
instance Layout l => Layout (D l) where
 data Name  (D l)               = D (Name l)
 type Index (D l)               = Index l
 name                           = D name
 create     (D n) len           = Delayed (create n len)
 extent     (Delayed l)         = extent l
 toIndex    (Delayed l) ix      = toIndex l ix
 fromIndex  (Delayed l) i       = fromIndex l i
 {-# INLINE_ARRAY name      #-}
 {-# INLINE_ARRAY create    #-}
 {-# INLINE_ARRAY extent    #-}
 {-# INLINE_ARRAY toIndex   #-}
 {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name l) => Eq   (Name (D l))
deriving instance Show (Name l) => Show (Name (D l))


-------------------------------------------------------------------------------
-- | Delayed arrays.
instance Layout l => Bulk (D l) a where
 data Array (D l) a
        = ADelayed !l (Index l -> a) 

 layout (ADelayed l _)      = Delayed l
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


-- Conversions ----------------------------------------------------------------
-- | Wrap a function as a delayed array.
--
--  @> toList $ fromFunction (Linear 10) (* 2)
--    = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]@
-- 
fromFunction :: l -> (Index l -> a) -> Array (D l) a
fromFunction l f 
        = ADelayed l f 
{-# INLINE_ARRAY fromFunction #-}


-- | Produce the extent of an array, and a function to retrieve an
--   arbitrary element.
toFunction  :: Bulk  l a
            => Array (D l) a -> (l, Index l -> a)
toFunction (ADelayed l f) = (l, f)
{-# INLINE_ARRAY toFunction #-}



-- Operators ------------------------------------------------------------------
-- | Wrap an existing array in a delayed one.
delay   :: Bulk l a
        => Array l a -> Array (D l) a
delay arr = map id arr
{-# INLINE delay #-}


-- | Apply a worker function to each element of an array, 
--   yielding a new array with the same extent.
map     :: Bulk l a
        => (a -> b) -> Array l a -> Array (D l) b
map f arr
        = ADelayed (layout arr) (f . index arr)
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
