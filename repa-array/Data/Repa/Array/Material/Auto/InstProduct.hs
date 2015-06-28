{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}
{-# LANGUAGE    UndecidableInstances #-}
module Data.Repa.Array.Material.Auto.InstProduct
where
import Data.Repa.Array.Material.Auto.Base       as A
import Data.Repa.Array.Material.Auto.InstUnit   as A
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Meta.Window              as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Scalar.Singleton.Nat           as D
import Data.Repa.Scalar.Product                 as D
import Data.Repa.Fusion.Unpack                  as F
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
 layout (AArray_Prod arrA arrB)    = Auto (min (A.length arrA) (A.length arrB))
 index  (AArray_Prod arrA arrB) ix = A.index arrA ix :*: A.index arrB ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


deriving instance (Show (Array A a), Show (Array A b))
                => Show (Array A (a :*: b))


instance (A.Windowable A a, A.Windowable A b)
      =>  A.Windowable A (a :*: b) where
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
instance IsProdList (Array A ()) where
 isProdList _ = True
 {-# INLINE isProdList #-}


instance IsProdList (Array A ts) 
      => IsProdList (Array A (f :*: ts)) where
 isProdList (AArray_Prod _ arr2) = isProdList arr2
 {-# INLINE isProdList #-}


-------------------------------------------------------------------------------------------- Select
instance IsProdList (Array A ts)
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
instance IsProdList (Array A ts)
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


---------------------------------------------------------------------------------------------------
-- | Form the product of two arrays, in constant time.
prod    :: Array A a -> Array A b -> Array A (a :*: b)
prod a1 a2 = AArray_Prod a1 a2
{-# INLINE prod #-}

-- | Unpack a product of two arrays, in constant time.
unprod  :: Array A (a :*: b) -> (Array A a, Array A b)
unprod (AArray_Prod a1 a2) = (a1, a2)
{-# INLINE unprod #-}


---------------------------------------------------------------------------------------------------
pattern Prod1 n a1     
        = AArray_Prod a1
         (AArray_Unit n)

pattern Prod2 n a1 a2
        = AArray_Prod a1 (AArray_Prod a2
         (AArray_Unit n))

pattern Prod3 n a1 a2 a3
        = AArray_Prod a1 (AArray_Prod a2 (AArray_Prod a3
         (AArray_Unit n)))

pattern Prod4 n a1 a2 a3 a4
        = AArray_Prod a1 (AArray_Prod a2 (AArray_Prod a3 (AArray_Prod a4 
         (AArray_Unit n))))

pattern Prod5 n a1 a2 a3 a4 a5 
        = AArray_Prod a1 (AArray_Prod a2 (AArray_Prod a3 (AArray_Prod a4 
         (AArray_Prod a5
         (AArray_Unit n)))))

pattern Prod6 n a1 a2 a3 a4 a5 a6
        = AArray_Prod a1 (AArray_Prod a2 (AArray_Prod a3 (AArray_Prod a4 
         (AArray_Prod a5 (AArray_Prod a6
         (AArray_Unit n))))))

pattern Prod7 n a1 a2 a3 a4 a5 a6 a7
        = AArray_Prod a1 (AArray_Prod a2 (AArray_Prod a3 (AArray_Prod a4 
         (AArray_Prod a5 (AArray_Prod a6 (AArray_Prod a7
         (AArray_Unit n)))))))

pattern Prod8 n a1 a2 a3 a4 a5 a6 a7 a8
        = AArray_Prod a1 (AArray_Prod a2 (AArray_Prod a3 (AArray_Prod a4 
         (AArray_Prod a5 (AArray_Prod a6 (AArray_Prod a7 (AArray_Prod a8
         (AArray_Unit n))))))))

pattern Prod9 n a1 a2 a3 a4 a5 a6 a7 a8 a9
        = AArray_Prod a1 (AArray_Prod a2 (AArray_Prod a3 (AArray_Prod a4 
         (AArray_Prod a5 (AArray_Prod a6 (AArray_Prod a7 (AArray_Prod a8
         (AArray_Prod a9 
         (AArray_Unit n)))))))))

