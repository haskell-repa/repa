
module Data.Array.Repa.Repr.HintSmall
        (S, Array (..), hintSmall)
where
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Base
import Data.Array.Repa.Shape


-- | Hints that evaluating this array is only a small amount of work.
--   It will be evaluated sequentially in the main thread, instead of
--   in parallel on the gang. This avoids the associated scheduling overhead.
data S r1

instance Source r1 a => Source (S r1) a where
 data Array (S r1) sh a
        = ASmall !(Array r1 sh a)

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


-- | Wrap an array with a smallness hint.
hintSmall :: Array r1 sh e -> Array (S r1) sh e
hintSmall = ASmall


deriving instance Show (Array r1 sh e) 
        => Show (Array (S r1) sh e)

deriving instance Read (Array r1 sh e) 
        => Read (Array (S r1) sh e)


-- Load ----------------------------------------------------------------------
instance ( Shape sh, Load r1 sh e) 
        => Load (S r1) sh e where
 loadP (ASmall arr) marr
  = loadS arr marr
 {-# INLINE loadP #-}

 loadS (ASmall arr) marr
  = loadS arr marr
 {-# INLINE loadS #-}


-- LoadRange ------------------------------------------------------------------
instance ( Shape sh, LoadRange r1 sh e)
        => LoadRange (S r1) sh e where
 loadRangeP (ASmall arr) marr ix1 ix2
  = loadRangeS arr marr ix1 ix2
 {-# INLINE loadRangeP #-}

 loadRangeS (ASmall arr) marr ix1 ix2
  = loadRangeS arr marr ix1 ix2
 {-# INLINE loadRangeS #-}
