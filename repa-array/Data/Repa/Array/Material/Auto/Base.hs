{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
module Data.Repa.Array.Material.Auto.Base
        ( A             (..)
        , Name          (..)
        , Array         (..))
where
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Generic.Convert          as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Material.Unboxed         as A
import Data.Repa.Array.Material.Foreign         as A
import Data.Repa.Array.Material.Nested          as A
import Data.Repa.Fusion.Unpack                  as F
import Data.Repa.Product                        as B
import Control.Monad
#include "repa-array.h"

-- | Arrays where the elements that are automatically layed out into some
--   efficient runtime representation.
--
--   The implementation uses type families to chose unboxed representations
--   for all elements that can be unboxed. In particular: arrays of unboxed
--   tuples are represented as tuples of unboxed arrays, and nested arrays
--   are represented using a segment descriptor and a single single flat
--   vector containing all the elements.
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


---------------------------------------------------------------------------------------------- Char
instance Bulk A Char where
 data Array A Char              = AArray_Char !(Array F Char)
 layout (AArray_Char arr)       = Auto (A.length arr)
 index  (AArray_Char arr) ix    = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


deriving instance Show (Array A Char)


instance A.Convert F Char A Char where
 convert arr = AArray_Char arr


instance A.Convert A Char F Char where
 convert (AArray_Char arr) = arr


instance Windowable A Char where
 window st len (AArray_Char arr) 
  = AArray_Char (window st len arr)
 {-# INLINE_ARRAY window #-}


instance Unpack (Array F Char) t 
      => Unpack (Array A Char) t where
 unpack (AArray_Char arr)   = unpack arr
 repack (AArray_Char x) arr = AArray_Char (repack x arr)


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
 {-# INLINE (==) #-}


----------------------------------------------------------------------------------------------- (,)
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
 {-# INLINE convert #-}


instance ( A.Convert A a1 A a2
         , A.Convert A b1 A b2)
        => A.Convert A (a1, b1) (T2 A A) (a2, b2) where
 convert (AArray_T2 (T2Array arrA arrB))
  = T2Array (convert arrA) (convert arrB)
 {-# INLINE convert #-}


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
 data Buffer A (a :*: b)            
  = ABuffer_Prod !(Buffer A a) !(Buffer A b)

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


instance ( Unpack (Buffer A a) tA
         , Unpack (Buffer A b) tB)
      =>   Unpack (Buffer A (a :*: b)) (tA, tB) where
 unpack (ABuffer_Prod bufA bufB)            
        = (unpack bufA, unpack bufB)

 repack (ABuffer_Prod xA   xB) (bufA, bufB) 
        = ABuffer_Prod (repack xA bufA) (repack xB bufB)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance (Eq (Array A a), Eq (Array A b))
       => Eq (Array A (a :*: b)) where
 (==) (AArray_Prod arrA1 arrA2) (AArray_Prod arrB1 arrB2) 
        = arrA1 == arrB1 && arrA2 == arrB2
 {-# INLINE (==) #-}


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
 data Buffer A [a]
  = ABuffer_List !(Buffer B [a])

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


instance Unpack (Buffer A String) (Buffer A String) where
 unpack buf   = buf
 repack _ buf = buf
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance Eq a
      => Eq (Array A [a]) where
 (==) (AArray_List arr1) (AArray_List arr2) = arr1 == arr2
 {-# INLINE (==) #-}


--------------------------------------------------------------------------------------------- Array
instance (Bulk A a, Windowable r a, Index r ~ Int)
       => Bulk A (Array r a) where
 data Array A (Array r a)       = AArray_Array !(Array N (Array r a))
 layout (AArray_Array arr)      = Auto (A.length arr)
 index  (AArray_Array arr) ix   = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index #-}

deriving instance Show (Array A a) => Show (Array A (Array A a))


instance Convert r1 a1 r2 a2
      => Convert  A (Array r1 a1)  N (Array r2 a2) where
 convert (AArray_Array (NArray starts lens arr)) 
        = NArray starts lens (convert arr)
 {-# INLINE convert #-}


instance Convert r1 a1 r2 a2
      => Convert  N (Array r1 a1)  A (Array r2 a2) where
 convert (NArray starts lens arr)
        = AArray_Array (NArray starts lens (convert arr))
 {-# INLINE convert #-}


instance Convert r1 a1 r2 a2
      => Convert A  (Array r1 a1) A (Array r2 a2) where
 convert (AArray_Array (NArray starts lens arr))
        = AArray_Array (NArray starts lens (convert arr))
 {-# INLINE convert #-}


instance (Bulk l a, Target l a, Index l ~ Int) 
       => Target A (Array l a) where
 data Buffer A (Array l a)
  = ABuffer_Array !(Buffer N (Array l a))

 unsafeNewBuffer (Auto len)     
  = liftM ABuffer_Array $ unsafeNewBuffer (Nested len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Array arr) ix
  = unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Array arr) ix x
  = unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Array arr) bump
  = liftM ABuffer_Array $ unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Array arr)
  = liftM AArray_Array  $ unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Array arr)
  = liftM ABuffer_Array $ unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Array buf)
  = liftM ABuffer_Array $ unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (ABuffer_Array buf)
  = touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Array buf)
  = Auto $ A.extent $ bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance Unpack (Buffer N (Array l a)) t
      => Unpack (Buffer A (Array l a)) t where
 unpack (ABuffer_Array buf)   = unpack buf
 repack (ABuffer_Array x) buf = ABuffer_Array (repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


instance (Bulk A a, Windowable l a, Index l ~ Int)
       => Windowable A (Array l a) where
 window st len (AArray_Array arr) 
  = AArray_Array (window st len arr)
 {-# INLINE_ARRAY window #-}

