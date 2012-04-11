
module Data.Array.Repa.Repr.Hint
        (S, Array (..)) 
where
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Shape

data S r1

data instance Array (S r1) sh e
        = ASmall !(Array r1 sh e)

instance Repr r1 a => Repr (S r1) a where
 extent (ASmall arr) 
        = extent arr
 {-# INLINE extent #-}

 index  (ASmall arr) ix
        = index arr ix
 {-# INLINE index #-}

 unsafeIndex (ASmall arr) ix
        = unsafeIndex arr ix
 {-# INLINE unsafeIndex #-}

 linearIndex (ASmall arr) ix
        = linearIndex arr ix
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (ASmall arr) ix
        = unsafeLinearIndex arr ix
 {-# INLINE unsafeLinearIndex #-}

 deepSeqArray (ASmall arr) x
        = deepSeqArray arr x
 {-# INLINE deepSeqArray #-}


-- Fill -----------------------------------------------------------------------
instance ( Shape sh, Fill r1 r2 sh e) 
        => Fill (S r1) r2 sh e where
 fillP (ASmall arr) marr
  = fillS arr marr
 {-# INLINE fillP #-}

 fillS (ASmall arr) marr
  = fillS arr marr
 {-# INLINE fillS #-}


-- FillRange ------------------------------------------------------------------
instance ( Shape sh, FillRange r1 r2 sh e)
        => FillRange (S r1) r2 sh e where
 fillRangeP (ASmall arr) marr ix1 ix2
  = fillRangeS arr marr ix1 ix2
 {-# INLINE fillRangeP #-}

 fillRangeS (ASmall arr) marr ix1 ix2
  = fillRangeS arr marr ix1 ix2
 {-# INLINE fillRangeS #-}
