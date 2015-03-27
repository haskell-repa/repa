{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.Base
        ( A             (..)
        , Name          (..)
        , Array         (..)
        , Foreign       (..))
where
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Material.Unboxed         as A
import Data.Repa.Array.Material.Foreign         as A
import Data.Repa.Array.Material.Nested          as A
import Data.Repa.Fusion.Unpack                  as F
import Data.Repa.Product                        as B
import Data.Word
import Data.Int
import Control.Monad
#include "repa-array.h"

-- | Automatic layout of array elements in some reasonably efficient
--   representation. 
--
--   The implementation uses type families to chose unboxed representations
--   for all elements that can be unboxed. In particular, arrays of unboxed
--   tuples are represented as tuples of unboxed arrays.
--
data A  = Auto { autoLength :: Int }
        deriving (Show, Eq)


instance Layout A where
 data Name  A                   = A
 type Index A                   = Int
 name                           = A
 create A len                   = Auto len
 extent (Auto len)              = len
 toIndex   _ ix                 = ix
 fromIndex _ ix                 = ix
 {-# INLINE_ARRAY name      #-}
 {-# INLINE_ARRAY create    #-}
 {-# INLINE_ARRAY extent    #-}
 {-# INLINE_ARRAY toIndex   #-}
 {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name A)
deriving instance Show (Name A)


-- | Class of array types where the elements are stored in foreign memory.
class Foreign arr a where
 toForeign   :: arr a -> Array F a
 fromForeign :: Array F a -> arr a


----------------------------------------------------------------------------------------------- Int
instance Bulk A Int where
 data Array A Int               = AArray_Int !(Array F Int)
 layout (AArray_Int arr)        = Auto (A.length arr)
 index  (AArray_Int arr) ix     = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

instance Foreign (Array A) Int where
 toForeign  (AArray_Int arr)    = arr
 fromForeign arr                = AArray_Int arr

deriving instance Show (Array A Int)

instance Windowable A Int where
 window st len (AArray_Int arr) 
  = AArray_Int (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int where
 data Buffer s A Int            
  = ABuffer_Int !(Buffer s F Int)

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


instance (Unpack (Buffer s F Int)) t 
      => (Unpack (Buffer s A Int)) t where
 unpack (ABuffer_Int buf)   = unpack buf
 repack (ABuffer_Int x) buf = ABuffer_Int (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


--------------------------------------------------------------------------------------------- Word8
instance Bulk A Word8 where
 data Array A Word8              = AArray_Word8 !(Array F Word8)
 layout (AArray_Word8 arr)       = Auto (A.length arr)
 index  (AArray_Word8 arr) ix    = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Word8)

instance Windowable A Word8 where
 window st len (AArray_Word8 arr) 
  = AArray_Word8 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Word8 where
 data Buffer s A Word8            
  = ABuffer_Word8 !(Buffer s F Word8)

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


instance (Unpack (Buffer s F Word8)) t 
      => (Unpack (Buffer s A Word8)) t where
 unpack (ABuffer_Word8 buf)   = unpack buf
 repack (ABuffer_Word8 x) buf = ABuffer_Word8 (repack x buf)
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

instance Windowable A Int8 where
 window st len (AArray_Int8 arr) 
  = AArray_Int8 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int8 where
 data Buffer s A Int8            
  = ABuffer_Int8 !(Buffer s F Int8)

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


instance (Unpack (Buffer s F Int8)) t 
      => (Unpack (Buffer s A Int8)) t where
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

instance Windowable A Int16 where
 window st len (AArray_Int16 arr) 
  = AArray_Int16 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int16 where
 data Buffer s A Int16            
  = ABuffer_Int16 !(Buffer s F Int16)

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


instance (Unpack (Buffer s F Int16)) t 
      => (Unpack (Buffer s A Int16)) t where
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

instance Windowable A Int32 where
 window st len (AArray_Int32 arr) 
  = AArray_Int32 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int32 where
 data Buffer s A Int32            
  = ABuffer_Int32 !(Buffer s F Int32)

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


instance (Unpack (Buffer s F Int32)) t 
      => (Unpack (Buffer s A Int32)) t where
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

instance Windowable A Int64 where
 window st len (AArray_Int64 arr) 
  = AArray_Int64 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Int64 where
 data Buffer s A Int64            
  = ABuffer_Int64 !(Buffer s F Int64)

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


instance (Unpack (Buffer s F Int64)) t 
      => (Unpack (Buffer s A Int64)) t where
 unpack (ABuffer_Int64 buf)   = unpack buf
 repack (ABuffer_Int64 x) buf = ABuffer_Int64 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


--------------------------------------------------------------------------------------------- Float
instance Bulk A Float where
 data Array A Float             = AArray_Float !(Array F Float)
 layout (AArray_Float arr)      = Auto (A.length arr)
 index  (AArray_Float arr) ix   = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Float)

instance Windowable A Float where
 window st len (AArray_Float arr) 
  = AArray_Float (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Float where
 data Buffer s A Float            
  = ABuffer_Float !(Buffer s F Float)

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


instance (Unpack (Buffer s F Float)) t 
      => (Unpack (Buffer s A Float)) t where
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

instance Windowable A Double where
 window st len (AArray_Double arr) 
  = AArray_Double (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Double where
 data Buffer s A Double            
  = ABuffer_Double !(Buffer s F Double)

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


instance (Unpack (Buffer s F Double)) t 
      => (Unpack (Buffer s A Double)) t where
 unpack (ABuffer_Double buf)   = unpack buf
 repack (ABuffer_Double x) buf = ABuffer_Double (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


---------------------------------------------------------------------------------------------- Char
instance Bulk A Char where
 data Array A Char              = AArray_Char !(Array F Char)
 layout (AArray_Char arr)       = Auto (A.length arr)
 index  (AArray_Char arr) ix    = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (Array A Char)

instance Windowable A Char where
 window st len (AArray_Char arr) 
  = AArray_Char (window st len arr)
 {-# INLINE_ARRAY window #-}

instance Target A Char where
 data Buffer s A Char            
  = ABuffer_Char !(Buffer s F Char)

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


instance (Unpack (Buffer s F Char)) t 
      => (Unpack (Buffer s A Char)) t where
 unpack (ABuffer_Char buf)   = unpack buf
 repack (ABuffer_Char x) buf = ABuffer_Char (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


----------------------------------------------------------------------------------------------- (,)
instance (Bulk A a, Bulk A b) => Bulk A (a, b) where
 data Array A (a, b)            = AArray_T2 !(Array (T2 A A) (a, b))
 layout (AArray_T2 arr)         = Auto (A.length arr)
 index  (AArray_T2 arr) ix      = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance (Show (Array (T2 A A) (a, b)))
                => Show (Array A (a, b))

instance (Windowable A a, Windowable A b)
      =>  Windowable A (a, b) where
 window st len (AArray_T2 arr) 
  = AArray_T2 (window st len arr)
 {-# INLINE_ARRAY window #-}

instance (Target A a, Target A b)
       => Target A (a, b) where
 data Buffer s A (a, b)            
  = ABuffer_T2 !(Buffer s (T2 A A) (a, b))

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


instance Unpack (Buffer s (T2 A A) (a, b)) t
      => Unpack (Buffer s A (a, b)) t where
 unpack (ABuffer_T2 buf)   = unpack buf
 repack (ABuffer_T2 x) buf = ABuffer_T2 (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


----------------------------------------------------------------------------------------------- :*:
instance (Bulk A a, Bulk A b) => Bulk A (a :*: b) where
 data Array A (a :*: b)            = AArray_Prod !(Array A a) !(Array A b)
 layout (AArray_Prod arrA _ )      = Auto (A.length arrA)
 index  (AArray_Prod arrA arrB) ix = A.index arrA ix :*: A.index arrB ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance (Show (Array A a), Show (Array A b))
                => Show (Array A (a :*: b))

instance (Windowable A a, Windowable A b)
      =>  Windowable A (a :*: b) where
 window st len (AArray_Prod arrA arrB) 
  = AArray_Prod (window st len arrA) (window st len arrB)
 {-# INLINE_ARRAY window #-}


instance (Target A a, Target A b)
       => Target A (a :*: b) where
 data Buffer s A (a :*: b)            
  = ABuffer_Prod !(Buffer s A a) !(Buffer s A b)

 unsafeNewBuffer l     
  = liftM2 ABuffer_Prod (unsafeNewBuffer l) (unsafeNewBuffer l)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Prod bufA bufB) ix
  = do  xA      <- unsafeReadBuffer bufA ix
        xB      <- unsafeReadBuffer bufB ix
        return  (xA :*: xB)
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Prod bufA bufB) ix (xA :*: xB)
  = do  unsafeWriteBuffer bufA ix xA
        unsafeWriteBuffer bufB ix xB
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Prod bufA bufB) bump
  = do  bufA'   <- unsafeGrowBuffer bufA bump
        bufB'   <- unsafeGrowBuffer bufB bump
        return  $ ABuffer_Prod bufA' bufB'
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Prod bufA bufB)
  = do  arrA    <- unsafeFreezeBuffer bufA
        arrB    <- unsafeFreezeBuffer bufB
        return  $ AArray_Prod arrA arrB
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Prod arrA arrB)
  = do  bufA    <- unsafeThawBuffer  arrA
        bufB    <- unsafeThawBuffer  arrB
        return  $  ABuffer_Prod bufA bufB
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Prod bufA bufB)
  = do  bufA'   <- unsafeSliceBuffer st len bufA
        bufB'   <- unsafeSliceBuffer st len bufB
        return  $  ABuffer_Prod bufA' bufB'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Prod bufA bufB)
  = do  touchBuffer bufA
        touchBuffer bufB
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Prod bufA _)
  =     bufferLayout bufA
 {-# INLINE_ARRAY bufferLayout #-}


instance ( Unpack (Buffer s A a) tA
         , Unpack (Buffer s A b) tB)
      =>   Unpack (Buffer s A (a :*: b)) (tA, tB) where
 unpack (ABuffer_Prod bufA bufB)            
        = (unpack bufA, unpack bufB)

 repack (ABuffer_Prod xA   xB) (bufA, bufB) 
        = ABuffer_Prod (repack xA bufA) (repack xB bufB)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


----------------------------------------------------------------------------------------------- []
instance Bulk A a => Bulk A [a] where
 data Array A [a]               = AArray_List !(Array B [a])
 layout (AArray_List arr)       = Auto (A.length arr)
 index  (AArray_List arr) ix    = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show a => Show (Array A [a])

instance Bulk A a => Windowable A [a] where
 window st len (AArray_List arr) 
  = AArray_List (window st len arr)
 {-# INLINE_ARRAY window #-}

instance  Target A [a] where
 data Buffer s A [a]
  = ABuffer_List !(Buffer s B [a])

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_List $ unsafeNewBuffer (Boxed len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_List arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_List arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_List arr) bump
  = liftM ABuffer_List  $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_List arr)
  = liftM AArray_List   $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_List arr)
  = liftM ABuffer_List  $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_List buf)
  = liftM ABuffer_List  $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_List buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_List buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance Unpack (Buffer s A String) (Buffer s A String) where
 unpack buf   = buf
 repack _ buf = buf
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


--------------------------------------------------------------------------------------------- Array
instance (Bulk A a, Windowable A a) 
       => Bulk A (Array A a) where
 data Array A (Array A a)       = AArray_Array !(Array N (Array A a))
 layout (AArray_Array arr)      = Auto (A.length arr)
 index  (AArray_Array arr) ix   = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index #-}

deriving instance Show (Array A a) => Show (Array A (Array A a))

instance (Bulk A a, Windowable A a)
       => Windowable A (Array A a) where
 window st len (AArray_Array arr) 
  = AArray_Array (window st len arr)
 {-# INLINE_ARRAY window #-}
