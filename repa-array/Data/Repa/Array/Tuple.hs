{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Tuple
        ( T2(..), Array(..)
        , tup2, untup2)
where
import Data.Repa.Array.Window
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Control.Monad
import Prelude                          hiding (zip, unzip)
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Tupled arrays where the components are unpacked and can have 
--   separate representations.
data T2 r1 r2 = T2 r1 r2


-- | Tupled arrays.
instance (Repr r1, Repr r2) => Repr (T2 r1 r2) where
 type Safe   (T2 r1 r2) = T2 (Safe   r1) (Safe   r2)
 type Unsafe (T2 r1 r2) = T2 (Unsafe r1) (Unsafe r2)
 repr = T2 repr repr
 {-# INLINE repr #-}


-- | Tupled arrays.
instance (Bulk r1 sh a, Bulk r2 sh b)
       => Bulk (T2 r1 r2) sh (a, b) where

 -- | INVARIANT: both components of the tupled array have the same shape.
 data Array (T2 r1 r2) sh (a, b)
        = T2Array sh (Array r1 sh a) (Array r2 sh b)

 index  (T2Array _  arr1 arr2) ix = (index arr1 ix, index arr2 ix)
 extent (T2Array sh _    _)       = sh
 safe   (T2Array sh arr1 arr2)    = T2Array sh (safe arr1)   (safe arr2)
 unsafe (T2Array sh arr1 arr2)    = T2Array sh (unsafe arr1) (unsafe arr2)
 {-# INLINE_ARRAY index #-}
 {-# INLINE_ARRAY extent #-}
 {-# INLINE_ARRAY safe #-}
 {-# INLINE_ARRAY unsafe #-}

deriving instance 
    (Show sh, Show (Array r1 sh a), Show (Array r2 sh b))
 =>  Show (Array (T2 r1 r2) sh (a, b))


-------------------------------------------------------------------------------
-- | Tupled windows.
instance (Window r1 DIM1 a, Window r2 DIM1 b)
      =>  Window (T2 r1 r2) DIM1 (a, b) where
 window start size (T2Array _sh arr1 arr2)
        = T2Array size (window start size arr1) (window start size arr2)
 {-# INLINE_ARRAY window #-}


-------------------------------------------------------------------------------
-- | Tupled buffers.
instance (Target r1 a t1, Target r2 b t2)
      =>  Target (T2 r1 r2) (a, b) (t1, t2) where

 data Buffer (T2 r1 r2) (a, b) 
        = T2Buffer !(Buffer r1 a) !(Buffer r2 b)

 unsafeNewBuffer len
  = liftM2 T2Buffer (unsafeNewBuffer len) (unsafeNewBuffer len)
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

 unsafeFreezeBuffer sh (T2Buffer buf1 buf2)
  = do  arr1    <- unsafeFreezeBuffer sh buf1
        arr2    <- unsafeFreezeBuffer sh buf2
        return  $  T2Array sh arr1 arr2
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
tup2    :: (Bulk r1 sh a, Bulk r2 sh b)
        => Array r1 sh a -> Array r2 sh b 
        -> Array (T2 r1 r2) sh (a, b)
tup2 arr1 arr2
        = T2Array (intersectDim (extent arr1) (extent arr2))
                  arr1 arr2
{-# INLINE_ARRAY tup2 #-}


-- | Untuple an array of tuples in to a tuple of arrays.
--
--   * The two returned components may have different extents, though they are
--     guaranteed to be at least as big as the argument array. This is the
--     key property that makes `untup2` different from `unzip`.
--
untup2  :: Array (T2 r1 r2) sh (a, b)
        -> (Array r1 sh a, Array r2 sh b)

untup2  (T2Array _ arr1 arr2)
        = (arr1, arr2)
{-# INLINE_ARRAY untup2 #-}



