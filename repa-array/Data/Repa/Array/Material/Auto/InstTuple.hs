{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.InstTuple where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Generic.Convert          as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Fusion.Unpack                  as A
import Control.Monad
#include "repa-array.h"


instance (Bulk A a, Bulk A b) => Bulk A (a, b) where
 data Array A (a, b)            = AArray_T2 !(Array (T2 A A) (a, b))
 layout (AArray_T2 arr)         = Auto (A.length arr)
 index  (AArray_T2 arr) ix      = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


deriving instance (Show (Array (T2 A A) (a, b)))
                => Show (Array A (a, b))


instance ( A.Convert A a1 A a2
         , A.Convert A b1 A b2)
        => A.Convert (T2 A A) (a1, b1) A (a2, b2) where
 convert (T2Array arrA arrB)    
  = AArray_T2 (T2Array (convert arrA) (convert arrB))
 {-# INLINE_ARRAY convert #-}


instance ( A.Convert A a1 A a2
         , A.Convert A b1 A b2)
        => A.Convert A (a1, b1) (T2 A A) (a2, b2) where
 convert (AArray_T2 (T2Array arrA arrB))
  = T2Array (convert arrA) (convert arrB)
 {-# INLINE_ARRAY convert #-}


instance (Windowable A a, Windowable A b)
      =>  Windowable A (a, b) where
 window st len (AArray_T2 arr) 
  = AArray_T2 (window st len arr)
 {-# INLINE_ARRAY window #-}


instance (Target A a, Target A b)
       => Target A (a, b) where
 data Buffer A (a, b)            
  = ABuffer_T2 !(Buffer (T2 A A) (a, b))

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_T2 $ unsafeNewBuffer (Tup2 (Auto len) (Auto len))
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_T2 arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_T2 arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_T2 arr) bump
  = liftM ABuffer_T2  $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_T2 arr)
  = liftM AArray_T2   $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_T2 arr)
  = liftM ABuffer_T2  $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_T2 buf)
  = liftM ABuffer_T2  $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_T2 buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_T2 buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance Unpack (Buffer (T2 A A) (a, b)) t
      => Unpack (Buffer A (a, b)) t where
 unpack (ABuffer_T2 buf)   = unpack buf
 repack (ABuffer_T2 x) buf = ABuffer_T2 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Eq (Array (T2 A A) (a, b))
      => Eq (Array A (a, b)) where
 (==) (AArray_T2 arr1) (AArray_T2 arr2) = arr1 == arr2
 {-# INLINE (==) #-}

