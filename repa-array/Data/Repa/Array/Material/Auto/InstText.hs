{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Data.Repa.Array.Material.Auto.InstText
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
import Data.Text                                (Text)
#include "repa-array.h"


----------------------------------------------------------------------------------------------- []
-- TODO: The current representation is just a boxed array of Text strings,
--       we want to unpack these into a segmented rep during freezing.
instance Bulk A Text where
 data Array A Text              = AArray_Text !(Array B Text)
 layout (AArray_Text arr)       = Auto (A.length arr)
 index  (AArray_Text arr) ix    = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Text)


instance Windowable A Text where
 window st len (AArray_Text arr) 
  = AArray_Text (window st len arr)
 {-# INLINE_ARRAY window #-}


instance  Target A Text where
 data Buffer A Text
  = ABuffer_Text !(Buffer B Text)

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Text $ unsafeNewBuffer (Boxed len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Text arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Text arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Text arr) bump
  = liftM ABuffer_Text  $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Text arr)
  = liftM AArray_Text   $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Text arr)
  = liftM ABuffer_Text  $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Text buf)
  = liftM ABuffer_Text  $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer  (ABuffer_Text buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Text buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance Unpack (Buffer A Text) (Buffer A Text) where
 unpack buf   = buf
 repack _ buf = buf
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Eq (Array A Text) where
 (==) (AArray_Text arr1) (AArray_Text arr2) = arr1 == arr2
 {-# INLINE (==) #-}
