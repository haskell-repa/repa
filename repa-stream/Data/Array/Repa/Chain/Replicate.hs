
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
-- | Given a chain of pairs containing a count an an element,
--   replicate element the number of times given by the count.
--
--   The first parameter sets the size of the resulting stream.
-- 
--   @
--   replicateEach 10 [(2,10), (5,20), (3,30)]
--     = [10,10,20,20,20,20,20,30,30,30]
--   @
--
replicateEach
        :: Int#                 -- ^ Total number of elements that will be produced.
        -> Chain (Int, a)       -- ^ Segment length and values.
        -> Chain a

replicateEach len (Chain _segs s0 next)
 = Chain len (ReplicateEachS 0# Nothing s0) next'
 where  
        -- move to the next segment.
        next' ix !(ReplicateEachS kElems mx s)
         | kElems ==# 0#
         = case next ix s of
                Yield  s' (I# kElems', !x)  
                 -> Update (ReplicateEachS kElems' (Just x)  s')

                Update s'
                 -> Update (ReplicateEachS 0#      Nothing   s')

         -- emit an element.
         | Just x       <- mx
         = Yield (ReplicateEachS (kElems -# 1#) mx s) x

         -- NOTE: this is never entered but we add the case to keep
         --       the GHC exhaustive match checker happy.
         | otherwise
         = Update (ReplicateEachS 0# mx s)
        {-# INLINE next' #-}
{-# INLINE [1] replicateEach #-}

data ReplicateEachS a b
        = ReplicateEachS Int# !(Maybe a) !b


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
