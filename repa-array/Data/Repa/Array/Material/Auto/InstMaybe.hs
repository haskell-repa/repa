{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Data.Repa.Array.Material.Auto.InstMaybe
        ( A             (..)
        , Name          (..)
        , Array         (..))
where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Fusion.Unpack                  as F
import Control.Monad
#include "repa-array.h"


instance Bulk A a => Bulk A (Maybe a) where
 data Array A (Maybe a)
        = AArray_Maybe !(Array B (Maybe a))

 layout (AArray_Maybe arr)
        = Auto (A.length arr)
 {-# INLINE_ARRAY layout #-}

 index  (AArray_Maybe arr) ix   
  = A.index arr ix
 {-# INLINE_ARRAY index  #-}

deriving instance Show a => Show (Array A (Maybe a))


instance Bulk A a => Windowable A (Maybe a) where
 window st len (AArray_Maybe arr) 
  = AArray_Maybe (window st len arr)
 {-# INLINE_ARRAY window #-}


instance  Target A (Maybe a) where
 data Buffer A (Maybe a)
  = ABuffer_Maybe !(Buffer B (Maybe a))

 unsafeNewBuffer (Auto len)
  = liftM ABuffer_Maybe $ unsafeNewBuffer (Boxed len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Maybe arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Maybe arr) ix !mx
  = unsafeWriteBuffer arr ix mx
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Maybe arr) bump
  = liftM ABuffer_Maybe $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Maybe arr)
  = liftM AArray_Maybe  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Maybe arr)
  = liftM ABuffer_Maybe $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Maybe buf)
  = liftM ABuffer_Maybe $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Maybe buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Maybe buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance Unpack (Buffer A (Maybe a)) (Buffer A (Maybe a)) where
 unpack buf   = buf
 repack _ buf = buf
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Eq a
      => Eq (Array A (Maybe a)) where
 (==) (AArray_Maybe arr1) (AArray_Maybe arr2) = arr1 == arr2
 {-# INLINE (==) #-}


