
module Data.Vector.Repa.Repr.Sliced
        ( S
        , Array (..)
        , vslice)
where
import Data.Vector.Repa.Base
import Data.Array.Repa                  as R
import Prelude  hiding (length)
import Prelude                          as P


data S r

instance Source r e => Source (S r) e where
 data Array (S r) sh e
        = ASliced 
                sh              --  starting position
                sh              --  extent of slice
                (Array r sh e)  --  source array

 extent (ASliced _ shape _)        
  = shape
 {-# INLINE extent #-}

 index (ASliced start _ arr) ix
  = start `deepSeq` unsafeIndex arr (addDim start ix)
 {-# INLINE index #-}

 linearIndex (ASliced start shape arr) ix
  = start `deepSeq` shape `deepSeq` 
    unsafeIndex arr 
        (addDim start (fromIndex shape ix))
 {-# INLINE linearIndex #-}

 deepSeqArray (ASliced start shape _) x
  = start `deepSeq` shape `deepSeq` x
 {-# INLINE deepSeqArray #-}


-- Map ------------------------------------------------------------------------
instance Map r e => Map (S r) e where
 type TM (S r) = S (TM r)

 vmap f (ASliced start shape arr)
  = ASliced start shape (vmap f arr)
 {-# INLINE [4] vmap #-}


-- Zips -----------------------------------------------------------------------
instance Zip r1 r2 a b
      => Zip (S r1) (S r2) a b where
 type TZ (S r1) (S r2) = S (TZ r1 r2)

 vzip  (ASliced start shape arr1)
        (ASliced _     _     arr2)
  = ASliced start shape (vzip arr1 arr2)
 {-# INLINE [4] vzip #-}



-------------------------------------------------------------------------------
-- | Slice a chunk from a vector.
vslice  :: Source r e 
        => Int                  -- ^ Starting index.
        -> Int                  -- ^ Length of slice.
        -> Vector r e 
        -> Vector (S r) e

vslice start len vec
 = ASliced (Z :. start) (Z :. len) vec
{-# INLINE [4] vslice #-}


