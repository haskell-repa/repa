{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE    UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.InstInt
where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Material.Foreign         as A
import Data.Repa.Array.Generic.Convert          as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Fusion.Unpack                  as F
import Data.Int
import Control.Monad
#include "repa-array.h"


----------------------------------------------------------------------------------------------- Int
instance Bulk A Int where
 data Array A Int               = AArray_Int !(Array F Int)
 layout (AArray_Int arr)        = Auto (A.length arr)
 index  (AArray_Int arr) ix     = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Int)

instance Convert F Int A Int where
 convert arr = AArray_Int arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Int F Int where
 convert (AArray_Int arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Int where
 window st len (AArray_Int arr) 
  = AArray_Int (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int where
 data Buffer A Int            
  = ABuffer_Int !(Buffer F Int)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Int $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Int arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Int arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Int arr) bump
  = liftM ABuffer_Int $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Int arr)
  = liftM AArray_Int  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Int arr)
  = liftM ABuffer_Int $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Int buf)
  = liftM ABuffer_Int $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Int buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Int buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Int)) t 
      => (Unpack (Buffer A Int)) t where
 unpack (ABuffer_Int buf)   = unpack buf
 repack (ABuffer_Int x) buf = ABuffer_Int (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


--------------------------------------------------------------------------------------------- Int8
instance Bulk A Int8 where
 data Array A Int8               = AArray_Int8 !(Array F Int8)
 layout (AArray_Int8 arr)        = Auto (A.length arr)
 index  (AArray_Int8 arr) ix     = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Int8)

instance Convert F Int8 A Int8 where
 convert arr = AArray_Int8 arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Int8 F Int8 where
 convert (AArray_Int8 arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Int8 where
 window st len (AArray_Int8 arr) 
  = AArray_Int8 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int8 where
 data Buffer A Int8            
  = ABuffer_Int8 !(Buffer F Int8)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Int8 $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Int8 arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Int8 arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Int8 arr) bump
  = liftM ABuffer_Int8 $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Int8 arr)
  = liftM AArray_Int8  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Int8 arr)
  = liftM ABuffer_Int8 $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Int8 buf)
  = liftM ABuffer_Int8 $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Int8 buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Int8 buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Int8)) t 
      => (Unpack (Buffer A Int8)) t where
 unpack (ABuffer_Int8 buf)   = unpack buf
 repack (ABuffer_Int8 x) buf = ABuffer_Int8 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


--------------------------------------------------------------------------------------------- Int16
instance Bulk A Int16 where
 data Array A Int16               = AArray_Int16 !(Array F Int16)
 layout (AArray_Int16 arr)        = Auto (A.length arr)
 index  (AArray_Int16 arr) ix     = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Int16)

instance Convert F Int16 A Int16 where
 convert arr = AArray_Int16 arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Int16 F Int16 where
 convert (AArray_Int16 arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Int16 where
 window st len (AArray_Int16 arr) 
  = AArray_Int16 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int16 where
 data Buffer A Int16            
  = ABuffer_Int16 !(Buffer F Int16)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Int16 $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Int16 arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Int16 arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Int16 arr) bump
  = liftM ABuffer_Int16 $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Int16 arr)
  = liftM AArray_Int16  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Int16 arr)
  = liftM ABuffer_Int16 $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Int16 buf)
  = liftM ABuffer_Int16 $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Int16 buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Int16 buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Int16)) t 
      => (Unpack (Buffer A Int16)) t where
 unpack (ABuffer_Int16 buf)   = unpack buf
 repack (ABuffer_Int16 x) buf = ABuffer_Int16 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


--------------------------------------------------------------------------------------------- Int32
instance Bulk A Int32 where
 data Array A Int32               = AArray_Int32 !(Array F Int32)
 layout (AArray_Int32 arr)        = Auto (A.length arr)
 index  (AArray_Int32 arr) ix     = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Int32)

instance Convert F Int32 A Int32 where
 convert arr = AArray_Int32 arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Int32 F Int32 where
 convert (AArray_Int32 arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Int32 where
 window st len (AArray_Int32 arr) 
  = AArray_Int32 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int32 where
 data Buffer A Int32            
  = ABuffer_Int32 !(Buffer F Int32)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Int32 $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Int32 arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Int32 arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Int32 arr) bump
  = liftM ABuffer_Int32 $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Int32 arr)
  = liftM AArray_Int32  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Int32 arr)
  = liftM ABuffer_Int32 $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Int32 buf)
  = liftM ABuffer_Int32 $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Int32 buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Int32 buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Int32)) t 
      => (Unpack (Buffer A Int32)) t where
 unpack (ABuffer_Int32 buf)   = unpack buf
 repack (ABuffer_Int32 x) buf = ABuffer_Int32 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


--------------------------------------------------------------------------------------------- Int64
instance Bulk A Int64 where
 data Array A Int64               = AArray_Int64 !(Array F Int64)
 layout (AArray_Int64 arr)        = Auto (A.length arr)
 index  (AArray_Int64 arr) ix     = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Int64)

instance Convert F Int64 A Int64 where
 convert arr = AArray_Int64 arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Int64 F Int64 where
 convert (AArray_Int64 arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Int64 where
 window st len (AArray_Int64 arr) 
  = AArray_Int64 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int64 where
 data Buffer A Int64            
  = ABuffer_Int64 !(Buffer F Int64)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Int64 $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Int64 arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Int64 arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Int64 arr) bump
  = liftM ABuffer_Int64 $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Int64 arr)
  = liftM AArray_Int64  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Int64 arr)
  = liftM ABuffer_Int64 $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Int64 buf)
  = liftM ABuffer_Int64 $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Int64 buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Int64 buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Int64)) t 
      => (Unpack (Buffer A Int64)) t where
 unpack (ABuffer_Int64 buf)   = unpack buf
 repack (ABuffer_Int64 x) buf = ABuffer_Int64 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}

