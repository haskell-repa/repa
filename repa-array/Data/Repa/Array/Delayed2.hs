{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Delayed2
        ( D2(..), Array(..)
        , delay2
        , map2)
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Load
import Data.Repa.Array.Internals.Target
import Debug.Trace
import GHC.Exts
import qualified Data.Repa.Eval.Generic.Par       as Par
import qualified Data.Repa.Eval.Generic.Seq       as Seq
#include "repa-array.h"


-------------------------------------------------------------------------------
-- | A delayed array formed from two source arrays.
--   The source arrays can have different layouts but must
--   have the same extent.
data D2 l1 l2
        = Delayed2
        { delayed2Layout1       :: l1
        , delayed2Layout2       :: l2 }

deriving instance (Eq   l1, Eq   l2) => Eq   (D2 l1 l2)
deriving instance (Show l1, Show l2) => Show (D2 l1 l2)


-------------------------------------------------------------------------------
-- | Delayed arrays.
instance (Layout l1, Layout l2, Index l1 ~ Index l2)
       => Layout (D2 l1 l2) where
 data Name  (D2 l1 l2)           = D2 (Name l1) (Name l2)
 type Index (D2 l1 l2)           = Index l1
 name                            = D2 name name
 create     (D2 n1 n2) len       = Delayed2 (create n1 len) (create n2 len)
 extent     (Delayed2 l1 _l2)    = extent    l1
 toIndex    (Delayed2 l1 _l2) ix = toIndex   l1 ix
 fromIndex  (Delayed2 l1 _l2) i  = fromIndex l1 i
 {-# INLINE_ARRAY name      #-}
 {-# INLINE_ARRAY create    #-}
 {-# INLINE_ARRAY extent    #-}
 {-# INLINE_ARRAY toIndex   #-}
 {-# INLINE_ARRAY fromIndex #-}

deriving instance 
        (Eq   (Name l1), Eq (Name l2)) 
      => Eq   (Name (D2 l1 l2))

deriving instance 
        (Show (Name l1), Show (Name l2)) 
     =>  Show (Name (D2 l1 l2))


-------------------------------------------------------------------------------
-- | Delayed arrays.
instance (Layout l1, Layout l2, Index l1 ~ Index l2)
       => Bulk (D2 l1 l2) a where

 data Array (D2 l1 l2) a
        = ADelayed2 !l1 !l2 (Index l1 -> a)

 layout (ADelayed2 l1 l2 _)     = Delayed2 l1 l2
 index  (ADelayed2 _  _  f) ix  = f ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index #-}


-- Load -----------------------------------------------------------------------
instance ( Layout lSrc1, Layout lSrc2, Target lDst a
         , Index  lSrc1 ~ Index lSrc2)
      =>  Load (D2 lSrc1 lSrc2) lDst a where

 loadS (ADelayed2 lSrc1 _lSrc2 get) !buf
  = do  let !(I# len)   = size (extent lSrc1)

        let write ix x  = unsafeWriteBuffer buf (I# ix) x
            get'  ix    = get (fromIndex lSrc1  (I# ix))
            {-# INLINE write #-}
            {-# INLINE get'  #-}

        Seq.fillLinear  write get' len
        touchBuffer  buf
 {-# INLINE_ARRAY loadS #-}

 loadP gang (ADelayed2 lSrc1 _lSrc2 get) !buf
  = do  traceEventIO "Repa.loadP[Delayed2]: start"
        let !(I# len)   = size (extent lSrc1)

        let write ix x  = unsafeWriteBuffer buf (I# ix) x
            get' ix     = get (fromIndex lSrc1  (I# ix))
            {-# INLINE write #-}
            {-# INLINE get'  #-}

        Par.fillChunked gang write get' len 
        touchBuffer  buf
        traceEventIO "Repa.loadP[Delayed2]: end"
 {-# INLINE_ARRAY loadP #-}


-- Operators ------------------------------------------------------------------
-- | Wrap two existing arrays in a delayed array.
delay2  :: (Bulk l1 a, Bulk l2 b, Index l1 ~ Index l2)
        => Array l1 a -> Array l2 b -> Maybe (Array (D2 l1 l2) (a, b))
delay2 arr1 arr2 = map2 (,) arr1 arr2
{-# INLINE delay2 #-}


-- | Combine two arrays element-wise using the given worker function.
--
--   The two source arrays must have the same extent, else `Nothing`.
map2    :: (Bulk l1 a, Bulk l2 b, Index l1 ~ Index l2)
        => (a -> b -> c) 
        -> Array l1 a -> Array l2 b
        -> Maybe (Array (D2 l1 l2) c)

map2 f arr1 arr2
 | extent (layout arr1) == extent (layout arr2)
 = let  get_map2 ix     = f (index arr1 ix) (index arr2 ix)
        {-# INLINE get_map2 #-}
   in   Just $ ADelayed2 (layout arr1) (layout arr2) get_map2

 | otherwise
 = Nothing
{-# INLINE_ARRAY map2 #-}


