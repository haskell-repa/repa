
module Data.Array.Repa.Repr.HintInterleave
        (I, Array (..), hintInterleave)
where
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Eval.Interleaved
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Debug.Trace


-- | Hints that computing this array will be an unbalanced workload
--   and evaluation should be interleaved between the processors.
data I r1

data instance Array (I r1) sh e
        = AInterleave !(Array r1 sh e)

deriving instance Show (Array r1 sh e) 
        => Show (Array (I r1) sh e)

deriving instance Read (Array r1 sh e) 
        => Read (Array (I r1) sh e)


-- | Wrap an array with a unbalanced-ness hint.
hintInterleave :: Array r1 sh e -> Array (I r1) sh e
hintInterleave = AInterleave


instance Source r1 sh a => Source (I r1) sh a where
 extent (AInterleave arr) 
        = extent arr
 {-# INLINE extent #-}

 index  (AInterleave arr) ix
        = index arr ix
 {-# INLINE index #-}

 unsafeIndex (AInterleave arr) ix
        = unsafeIndex arr ix
 {-# INLINE unsafeIndex #-}

 linearIndex (AInterleave arr) ix
        = linearIndex arr ix
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (AInterleave arr) ix
        = unsafeLinearIndex arr ix
 {-# INLINE unsafeLinearIndex #-}

 deepSeqArray (AInterleave arr) x
        = deepSeqArray arr x
 {-# INLINE deepSeqArray #-}


-- Fill -----------------------------------------------------------------------
instance ( Shape sh, Fill D r2 sh e) 
        => Fill (I D) r2 sh e where
 fillP (AInterleave (ADelayed sh getElem)) marr
  = marr `deepSeqMArr`
    do  traceEventIO "Repa.fillP[Interleaved]: start"
        fillInterleavedP (size sh) (unsafeWriteMArr marr) (getElem . fromIndex sh) 
        touchMArr marr
        traceEventIO "Repa.fillP[Interleaved]: end"
 {-# INLINE [4] fillP #-}

 -- The fact that the workload is unbalanced doesn't affect us when the
 -- program is run sequentially, so just use the filling method of the inner
 -- representation
 fillS (AInterleave arr) marr
  = fillS arr marr
 {-# INLINE fillS #-}
