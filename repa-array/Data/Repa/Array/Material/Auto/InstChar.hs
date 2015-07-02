{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.InstChar where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Material.Foreign         as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Generic.Convert          as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Fusion.Unpack                  as A
import Control.Monad
#include "repa-array.h"


instance Bulk A Char where
 data Array A Char              = AArray_Char !(Array F Char)
 layout (AArray_Char arr)       = Auto (A.length arr)
 index  (AArray_Char arr) ix    = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


deriving instance Show (Array A Char)


instance A.Convert F Char A Char where
 convert arr = AArray_Char arr
 {-# INLINE_ARRAY convert #-}


instance A.Convert A Char F Char where
 convert (AArray_Char arr) = arr
 {-# INLINE_ARRAY convert #-}


instance Windowable A Char where
 window st len (AArray_Char arr) 
  = AArray_Char (window st len arr)
 {-# INLINE_ARRAY window #-}


instance Unpack (Array F Char) t 
      => Unpack (Array A Char) t where
 unpack (AArray_Char arr)   = unpack arr
 repack (AArray_Char x) arr = AArray_Char (repack x arr)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Target A Char where
 data Buffer A Char            
  = ABuffer_Char !(Buffer F Char)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Char $ unsafeNewBuffer (Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Char arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Char arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Char arr) bump
  = liftM ABuffer_Char $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Char arr)
  = liftM AArray_Char  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Char arr)
  = liftM ABuffer_Char $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Char buf)
  = liftM ABuffer_Char $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Char buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Char buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer F Char)) t 
      => (Unpack (Buffer A Char)) t where
 unpack (ABuffer_Char buf)   = unpack buf
 repack (ABuffer_Char x) buf = ABuffer_Char (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Eq (Array A Char) where
 (==) (AArray_Char arr1) (AArray_Char arr2) = arr1 == arr2
 {-# INLINE_ARRAY (==) #-}

