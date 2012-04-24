
module Data.Array.Repa.Repr.Hint
        (S, Array (..), hintSmall)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Shape

-- | Hints that evaluating this array is only a small amount of work.
--   It will be evaluated sequentially in the main thread, instead of
--   in parallel on the gang. This avoids the associated scheduling overhead.
data S r1

data instance Array (S r1) sh e
        = ASmall !(Array r1 sh e)

deriving instance Show (Array r1 sh e) 
        => Show (Array (S r1) sh e)

deriving instance Read (Array r1 sh e) 
        => Read (Array (S r1) sh e)


-- | Wrap an array with a smallness hint.
hintSmall :: Array r1 sh e -> Array (S r1) sh e
hintSmall = ASmall


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
