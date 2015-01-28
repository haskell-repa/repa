{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Tuple
        ( T2(..), Array(..)
        , tup2, untup2)
where
import Data.Repa.Array.Window
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Control.Monad
import Prelude                          hiding (zip, unzip)
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Tupled arrays where the components are unpacked and can have 
--   separate representations.
data T2 l1 l2 = T2 l1 l2


instance ( Index  l1 ~ Index l2
         , Layout l1, Layout l2) 
        => Layout (T2 l1 l2) where

        type Index (T2 l1 l2)    = Index l1

        extent    (T2 l1 l2)     = intersectDim (extent l1) (extent l2)
        {-# INLINE extent #-}

        toIndex   (T2 l1 _l2) ix = toIndex l1 ix
        {-# INLINE toIndex #-}

        fromIndex (T2 l1 _l2) ix = fromIndex l1 ix
        {-# INLINE fromIndex #-}


-- | Tupled arrays.
instance (Bulk l1 a, Bulk l2 b, Index l1 ~ Index l2)
       => Bulk (T2 l1 l2) (a, b) where

 -- | INVARIANT: both components of the tupled array have the same shape.
 data Array (T2 l1 l2) (a, b)
        = T2Array (Array l1 a) (Array l2 b)

 layout (T2Array arr1 arr2)     = T2 (layout arr1)  (layout arr2)
 index  (T2Array arr1 arr2) ix  = (index  arr1 ix, index  arr2 ix)
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


deriving instance 
    (Show (Array l1 a), Show (Array l2 b))
 =>  Show (Array (T2 l1 l2) (a, b))


-------------------------------------------------------------------------------
-- | Tupled windows.
instance (Windowable l1 a, Windowable l2 b, Index l1 ~ Index l2)
      =>  Windowable (T2 l1 l2) (a, b) where
 window st sz (T2Array arr1 arr2)
        = T2Array (window st sz arr1) (window st sz arr2)
 {-# INLINE_ARRAY window #-}


-------------------------------------------------------------------------------
-- | Tupled buffers.
instance ( Target l1 a, Target l2 b
         , Index l1 ~ Index l2)
      =>   Target (T2 l1 l2) (a, b) where

 data Buffer (T2 l1 l2) (a, b) 
        = T2Buffer !(Buffer l1 a) !(Buffer l2 b)

 unsafeNewBuffer (T2 l1 l2)
  = liftM2 T2Buffer (unsafeNewBuffer l1) (unsafeNewBuffer l2)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeWriteBuffer  (T2Buffer buf1 buf2) ix (x1, x2)
  = do  unsafeWriteBuffer buf1 ix x1
        unsafeWriteBuffer buf2 ix x2
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (T2Buffer buf1 buf2) bump
  = do  buf1'   <- unsafeGrowBuffer buf1 bump
        buf2'   <- unsafeGrowBuffer buf2 bump
        return  $  T2Buffer buf1' buf2'
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (T2Buffer buf1 buf2)
  = do  arr1    <- unsafeFreezeBuffer buf1
        arr2    <- unsafeFreezeBuffer buf2
        return  $  T2Array arr1 arr2
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeSliceBuffer start len (T2Buffer buf1 buf2)
  = do  buf1'   <- unsafeSliceBuffer start len buf1
        buf2'   <- unsafeSliceBuffer start len buf2
        return  $  T2Buffer buf1' buf2'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (T2Buffer buf1 buf2)
  = do  touchBuffer buf1        
        touchBuffer buf2
 {-# INLINE_ARRAY touchBuffer #-}


instance (Unpack (Buffer r1 a) t1, Unpack (Buffer r2 b) t2)
       => Unpack (Buffer (T2 r1 r2) (a, b)) (t1, t2) where
 unpack  (T2Buffer buf1 buf2) 
   = buf1 `seq` buf2 `seq` (unpack buf1, unpack buf2)
 {-# INLINE_ARRAY unpack #-}

 repack !(T2Buffer x1 x2) (buf1, buf2)      
   = buf1 `seq` buf2 `seq` (T2Buffer (repack x1 buf1) (repack x2 buf2))
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | Tuple two arrays into an array of pairs.
-- 
--   The extent of the result array is the intersection of the extents of the
--   two argument arrays.
--
tup2    :: (Bulk l1 a, Bulk l2 b)
        => Array l1 a -> Array l2 b 
        -> Array (T2 l1 l2) (a, b)
tup2 arr1 arr2
        = T2Array arr1 arr2
{-# INLINE_ARRAY tup2 #-}


-- | Untuple an array of tuples in to a tuple of arrays.
--
--   * The two returned components may have different extents, though they are
--     guaranteed to be at least as big as the argument array. This is the
--     key property that makes `untup2` different from `unzip`.
--
untup2  ::  Array (T2 l1 l2) (a, b)
        -> (Array l1 a, Array l2 b)

untup2  (T2Array arr1 arr2)
        = (arr1, arr2)
{-# INLINE_ARRAY untup2 #-}


