{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE    UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.InstFloat
where
import Data.Repa.Array.Generic.Convert          as A
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Material.Foreign         as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Fusion.Unpack                  as F
import Control.Monad
#include "repa-array.h"

--------------------------------------------------------------------------------------------- Float
instance Bulk A Float where
 data Array A Float             = AArray_Float !(Array F Float)
 layout (AArray_Float arr)      = Auto (A.length arr)
 index  (AArray_Float arr) ix   = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Float)

instance Convert F Float A Float where
 convert arr = AArray_Float arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Float F Float where
 convert (AArray_Float arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Float where
 window st len (AArray_Float arr) 
  = AArray_Float (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Float where
 data Buffer A Float            
  = ABuffer_Float !(Buffer F Float)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Float $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Float arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Float arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Float arr) bump
  = liftM ABuffer_Float $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Float arr)
  = liftM AArray_Float  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Float arr)
  = liftM ABuffer_Float $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Float buf)
  = liftM ABuffer_Float $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Float buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Float buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Float)) t 
      => (Unpack (Buffer A Float)) t where
 unpack (ABuffer_Float buf)   = unpack buf
 repack (ABuffer_Float x) buf = ABuffer_Float (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-------------------------------------------------------------------------------------------- Double
instance Bulk A Double where
 data Array A Double             = AArray_Double !(Array F Double)
 layout (AArray_Double arr)      = Auto (A.length arr)
 index  (AArray_Double arr) ix   = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Double)

instance Convert F Double A Double where
 convert arr = AArray_Double arr
 {-# INLINE_ARRAY convert #-}

instance Convert A Double F Double where
 convert (AArray_Double arr) = arr
 {-# INLINE_ARRAY convert #-}

instance Windowable A Double where
 window st len (AArray_Double arr) 
  = AArray_Double (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Double where
 data Buffer A Double            
  = ABuffer_Double !(Buffer F Double)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Double $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Double arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Double arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Double arr) bump
  = liftM ABuffer_Double $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Double arr)
  = liftM AArray_Double  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Double arr)
  = liftM ABuffer_Double $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Double buf)
  = liftM ABuffer_Double $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Double buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Double buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Double)) t 
      => (Unpack (Buffer A Double)) t where
 unpack (ABuffer_Double buf)   = unpack buf
 repack (ABuffer_Double x) buf = ABuffer_Double (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}
