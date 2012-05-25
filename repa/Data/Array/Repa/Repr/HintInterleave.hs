
module Data.Array.Repa.Repr.HintInterleave
        (I, Array (..), hintInterleave)
where
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Eval.Target
import Data.Array.Repa.Eval.Interleaved
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Debug.Trace


-- | Hints that computing this array will be an unbalanced workload
--   and evaluation should be interleaved between the processors.
data I r1

instance Source r1 a => Source (I r1) a where
 data Array (I r1) sh a
        = AInterleave !(Array r1 sh a)

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


deriving instance Show (Array r1 sh e) 
        => Show (Array (I r1) sh e)

deriving instance Read (Array r1 sh e) 
        => Read (Array (I r1) sh e)


-- | Wrap an array with a unbalanced-ness hint.
hintInterleave :: Array r1 sh e -> Array (I r1) sh e
hintInterleave = AInterleave


-- Load -----------------------------------------------------------------------
instance (Shape sh, Load D sh e) 
        => Load (I D) sh e where
 loadP (AInterleave (ADelayed sh getElem)) marr
  = marr `deepSeqMVec`
    do  traceEventIO "Repa.loadP[Interleaved]: start"
        fillInterleavedP (size sh) (unsafeWriteMVec marr) (getElem . fromIndex sh) 
        touchMVec marr
        traceEventIO "Repa.loadP[Interleaved]: end"
 {-# INLINE [4] loadP #-}

 -- The fact that the workload is unbalanced doesn't affect us when the
 -- program is run sequentially, so just use the filling method of the inner
 -- representation
 loadS (AInterleave arr) marr
  = loadS arr marr
 {-# INLINE loadS #-}

