{-# LANGUAGE    UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Data.Repa.Array.Material.Auto.InstDate32
        (rangeDate32)
where
import Data.Repa.Array.Material.Auto.InstChar           ()
import qualified Data.Repa.Array.Material.Auto.Base     as A
import qualified Data.Repa.Array.Material.Foreign       as A
import qualified Data.Repa.Array.Internals.Target       as A
import qualified Data.Repa.Array.Generic.Index          as A
import qualified Data.Repa.Array.Generic                as A
import qualified Data.Repa.Array.Meta.Window            as A
import qualified Data.Repa.Fusion.Unpack                as A
import Data.Repa.Scalar.Date32
import Control.Monad
import Prelude                                          as P
#include "repa-array.h"


instance A.Bulk A.A Date32 where
 data Array A.A Date32           = AArray_Date32 !(A.Array A.F Date32)
 layout (AArray_Date32 arr)      = A.Auto (A.length arr)
 index  (AArray_Date32 arr) ix   = A.index arr ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}

deriving instance Show (A.Array A.A Date32)


instance A.Windowable A.A Date32 where
 window st len (AArray_Date32 arr) 
  = AArray_Date32 (A.window st len arr)
 {-# INLINE_ARRAY window #-}


instance A.Target A.A Date32 where
 data Buffer A.A Date32            
  = ABuffer_Date32 !(A.Buffer A.F Date32)

 unsafeNewBuffer    (A.Auto len)     
  = liftM ABuffer_Date32 $ A.unsafeNewBuffer (A.Foreign len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer   (ABuffer_Date32 arr) ix
  = A.unsafeReadBuffer arr ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer  (ABuffer_Date32 arr) ix x
  = A.unsafeWriteBuffer arr ix x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer   (ABuffer_Date32 arr) bump
  = liftM ABuffer_Date32 $ A.unsafeGrowBuffer arr bump
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (ABuffer_Date32 arr)
  = liftM AArray_Date32  $ A.unsafeFreezeBuffer arr 
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer   (AArray_Date32 arr)
  = liftM ABuffer_Date32 $ A.unsafeThawBuffer  arr
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (ABuffer_Date32 buf)
  = liftM ABuffer_Date32 $ A.unsafeSliceBuffer st len buf
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer  (ABuffer_Date32 buf)
  = A.touchBuffer buf
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (ABuffer_Date32 buf)
  = A.Auto $ A.extent $ A.bufferLayout buf
 {-# INLINE_ARRAY bufferLayout #-}


instance (A.Unpack (A.Buffer A.F Date32)) t 
      => (A.Unpack (A.Buffer A.A Date32)) t where
 unpack (ABuffer_Date32 buf)   = A.unpack buf
 repack (ABuffer_Date32 x) buf = ABuffer_Date32 (A.repack x buf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


---------------------------------------------------------------------------------------------------
-- | Yield an array containing a range of dates, inclusive of the end points.
---
--   TODO: avoid going via lists.
--
rangeDate32   :: Date32 -> Date32 -> A.Array A.A Date32
rangeDate32 from to 
 | to < from    = A.fromList A.A []
 | otherwise    = A.fromList A.A $ go [] from
 where
        go !acc !d   
                | d > to        = P.reverse acc
                | otherwise     = go (d : acc) (next d)
{-# NOINLINE rangeDate32 #-}

