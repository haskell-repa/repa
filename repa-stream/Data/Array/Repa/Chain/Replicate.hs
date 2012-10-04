
module Data.Array.Repa.Chain.Replicate 
        ( replicate,            replicateD
        , replicateEach,        replicateEachD)
where
import Data.Array.Repa.Chain.Base
import GHC.Exts
import Prelude                  hiding (replicate)


-- Replicate ------------------------------------------------------------------
-- | Fill a chain with the same value.
replicate 
        :: Int#                 -- ^ Total number of elements to produce.
        -> a                    -- ^ Value to replicate.
        -> Chain a

replicate n x
 = Chain n () next
 where  next _ _ = Yield () x
        {-# INLINE next #-}
{-# INLINE replicate #-}


-- | Fill a distributed chain with the same value.
replicateD 
        :: Distro               -- ^ Distribution of result.
        -> a                    -- ^ Value to replicate.
        -> DistChain a

replicateD d x
 = DistChain d frag
 where  frag i
         = replicate (distroFragLength d i) x
        {-# INLINE frag #-}
{-# INLINE [1] replicateD #-}


-- ReplicateEach --------------------------------------------------------------
-- | Replicate several values the given number of times each.
replicateEach
        :: Int#                 -- ^ Total number of elements that will be produced.
        -> Chain (Int, a)       -- ^ Segment length and values.
        -> Chain a

replicateEach len (Chain _segs s0 next)
 = Chain len (0, Nothing, s0) next'
 where  
        -- move to the next segment.
        next' ix (kElems, mx, s)
         | kElems == 0
         = case next ix s of
                Yield  s' (!kElems', !x)  -> Update (kElems', Just x,  s')
                Update s'                 -> Update (0,       Nothing, s')

         -- emit an element.
         | Just x       <- mx
         = Yield (kElems - 1, mx, s) x

         -- NOTE: this is never entered but we add the case to keep
         --       the GHC exhaustive match checker happy.
         | otherwise
         = Update (0, mx, s)
        {-# INLINE next' #-}
{-# INLINE [1] replicateEach #-}


-- | Fill a distributed chain with several values the given number of times each.
replicateEachD 
        :: Distro               -- ^ Distribution of result.
        -> DistChain (Int, a)   -- ^ Segment length and values.
        -> DistChain a

replicateEachD d1 (DistChain _d2 frag2)
 = DistChain d1 frag1
 where  frag1 i
         = replicateEach 
                (distroFragLength d1 i)
                (frag2 i)
        {-# INLINE frag1 #-}
{-# INLINE [1] replicateEachD #-}
