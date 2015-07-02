{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Array.Material.Auto.InstUnit where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Fusion.Unpack                  as A
#include "repa-array.h"


instance Bulk A () where
 data Array   A ()              = AArray_Unit !Int
 layout (AArray_Unit len)       = Auto len
 index  (AArray_Unit _len) _ix  = ()
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A ())


instance Windowable A () where
 window _st len' (AArray_Unit _len)
  = AArray_Unit len'


instance Unpack (Array A ()) Int where
 unpack (AArray_Unit len)       = len
 repack (AArray_Unit _) len     = AArray_Unit len
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Target A () where
 data Buffer A ()           
  = ABuffer_Unit !Int

 unsafeNewBuffer (Auto len)     
  = return $ ABuffer_Unit len
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Unit _len) _ix
  = return ()
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Unit _len) _ix _x
  = return ()
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Unit len) bump
  = return $ ABuffer_Unit (len + bump)
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Unit len)
  = return $ AArray_Unit len 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Unit len)
  = return $ ABuffer_Unit len
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer _st len (ABuffer_Unit _len)
  = return $ ABuffer_Unit len
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Unit _len)
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Unit len)
  = Auto len
 {-# INLINE_ARRAY bufferLayout #-}


instance (Unpack (Buffer A ())) Int where
 unpack   (ABuffer_Unit len) = len
 repack _ len                = ABuffer_Unit len
 {-# INLINE unpack #-}
 {-# INLINE repack #-}
