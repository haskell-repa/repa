{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE    UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.InstProduct
where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Fusion.Unpack                  as F
import Data.Repa.Singleton.Nat                  as D
import Data.Repa.Product                        as D
import Control.Monad
#include "repa-array.h"


---------------------------------------------------------------------------------------------------
-- Arrays of products are stored in columnar format to allow easy selecting
-- and addition of columns. 
-- 
-- Given an array of type (Array A (Int :*: Char :*: Bool :*: ())), this is
-- stored as a flat array of Int, a flat array of Char, and a flat array of
-- Bool. The unit colunm is stored as a simple count of the number of elements, 
-- and serves as the Nil element in the list of column types.
--
-- Extracting particular column, getting the array of type (Array A Char)
-- is linear in the number of columns. Doing so takes about as long as
-- retrieving a single element from a cons-list.

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


--------------------------------------------------------------------------------------------- Valid
instance Valid (Array A ()) where
 valid _ = True
 {-# INLINE valid #-}


instance Valid (Array A ts) 
      => Valid (Array A (f :*: ts)) where
 valid (AArray_Prod _ arr2) = valid arr2
 {-# INLINE valid #-}


-------------------------------------------------------------------------------------------- Select
instance Valid (Array A ts)
      => Select 'Z (Array A (t1 :*: ts)) where
 type Select'   'Z (Array A (t1 :*: ts)) = Array A t1
 select       Zero (AArray_Prod x1 _)    = x1
 {-# INLINE select #-}


instance Select n (Array A ts)
      => Select ('S n) (Array A (t1 :*: ts)) where
 type Select'   ('S n) (Array A (t1 :*: ts)) = Select' n (Array A ts)
 select       (Succ n) (AArray_Prod _ xs)    = select  n xs
 {-# INLINE select #-}


------------------------------------------------------------------------------------------- Discard
instance Valid (Array A ts)
      => Discard 'Z    (Array A (t1 :*: ts)) where
 type Discard'   'Z    (Array A (t1 :*: ts)) = Array A ts
 discard       Zero    (AArray_Prod _ xs)    = xs
 {-# INLINE discard #-}


instance ( Discard  n   (Array A ts)
        ,  Discard' n   (Array A ts) ~ Array A (Discard' n ts))
      => Discard ('S n) (Array A (t1 :*: ts)) where
 type Discard'   ('S n) (Array A (t1 :*: ts)) = Array A (t1 :*: Discard' n ts)
 discard      (Succ n) (AArray_Prod x xs)     = AArray_Prod x (discard n xs)
 {-# INLINE discard #-}


---------------------------------------------------------------------------------------------- Mask
instance Mask () (Array A ()) where
 type Mask' ()   (Array A ()) = Array A ()
 mask       ()   arr          = arr
 {-# INLINE mask #-}


instance ( Mask  ms (Array A ts)
         , Mask' ms (Array A ts) ~ Array A (Mask' ms ts))
      =>   Mask (Keep :*: ms) (Array A (t1 :*: ts)) where
 type Mask'     (Keep :*: ms) (Array A (t1 :*: ts)) = Array A (t1 :*: Mask' ms ts)
 mask           (Keep :*: ms) (AArray_Prod x1 xs)   = AArray_Prod x1 (mask  ms xs)
 {-# INLINE mask #-}


instance ( Mask  ms (Array A ts))
      =>   Mask   (Drop :*: ms) (Array A (t1 :*: ts)) where
 type Mask'     (Drop :*: ms) (Array A (t1 :*: ts)) = Mask' ms (Array A ts)
 mask           (Drop :*: ms) (AArray_Prod _ xs)    = mask ms xs
 {-# INLINE mask #-}

