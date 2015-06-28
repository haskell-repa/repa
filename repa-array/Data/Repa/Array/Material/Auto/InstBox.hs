{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Data.Repa.Array.Material.Auto.InstBox where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Fusion.Unpack                  as F
import Data.Repa.Scalar.Box
import Control.Monad
#include "repa-array.h"


instance Bulk A a => Bulk A (Box a) where
 data Array A (Box a)           = AArray_Box !(Array B a)
 layout (AArray_Box arr)        = Auto (A.length arr)
 index  (AArray_Box arr) !ix    = Box  (A.index arr ix)
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show a => Show (Array A (Box a))


instance Bulk A a => Windowable A (Box a) where
 window st len (AArray_Box arr) 
  = AArray_Box (window st len arr)
 {-# INLINE_ARRAY window #-}


instance  Target A (Box a) where
 data Buffer A (Box a)
  = ABuffer_Box !(Buffer B a)

 unsafeNewBuffer (Auto len)
  = liftM ABuffer_Box $ unsafeNewBuffer (Boxed len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Box arr) ix
  = do  x       <- unsafeReadBuffer arr ix
        return  $ Box x
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Box arr) ix (Box x)
  = x `seq` unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Box arr) bump
  = liftM ABuffer_Box $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Box arr)
  = liftM AArray_Box  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Box arr)
  = liftM ABuffer_Box $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Box buf)
  = liftM ABuffer_Box $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Box buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Box buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance Unpack (Buffer A (Box a)) (Buffer A (Box a)) where
 unpack buf   = buf
 repack _ buf = buf
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Eq a
      => Eq (Array A (Box a)) where
 (==) (AArray_Box arr1) (AArray_Box arr2) = arr1 == arr2
 {-# INLINE_ARRAY (==) #-}

