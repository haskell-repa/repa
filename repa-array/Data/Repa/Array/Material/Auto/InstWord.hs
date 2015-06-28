{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE    UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.InstWord
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
import Data.Word
import Control.Monad
#include "repa-array.h"


--------------------------------------------------------------------------------------------- Word8
instance Bulk A Word8 where
 data Array A Word8              = AArray_Word8 !(Array F Word8)
 layout (AArray_Word8 arr)       = Auto (A.length arr)
 index  (AArray_Word8 arr) ix    = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

instance (Unpack (Array F Word8)) t 
      => (Unpack (Array A Word8)) t where
 unpack (AArray_Word8 buf)   = unpack buf
 repack (AArray_Word8 x) buf = AArray_Word8 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}

deriving instance Show (Array A Word8)

instance Convert F Word8 A Word8 where
 convert arr = AArray_Word8 arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Word8 F Word8 where
 convert (AArray_Word8 arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Word8 where
 window st len (AArray_Word8 arr) 
  = AArray_Word8 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Word8 where
 data Buffer A Word8            
  = ABuffer_Word8 !(Buffer F Word8)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Word8 $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Word8 arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Word8 arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Word8 arr) bump
  = liftM ABuffer_Word8 $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Word8 arr)
  = liftM AArray_Word8  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Word8 arr)
  = liftM ABuffer_Word8 $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Word8 buf)
  = liftM ABuffer_Word8 $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Word8 buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Word8 buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Word8)) t 
      => (Unpack (Buffer A Word8)) t where
 unpack (ABuffer_Word8 buf)   = unpack buf
 repack (ABuffer_Word8 x) buf = ABuffer_Word8 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}
