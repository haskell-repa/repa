
module Data.Array.Repa.Flow.Seq.Base
        ( FD, FS
        , Size          (..)
        , sizeMin)
where
import GHC.Exts


-------------------------------------------------------------------------------
-- | Type index to indicate a delayed source or sink.
--   Being delayed means the source\/sink does not yet have any attached state.
--
--   A delayed source may be attached to manifest vectors, but it it does not
--   yet have indices into these vectors. We can evaluate a delayed source
--   multiple times and get the same result.
--
--   A delayed sink has not yet initialised its accumulators.
--   We can push multiple flows into a sink to construct the same output.
--
data FD


-- | Type index to indicate a stateful (active) source or sink.
--   
--   Pulling from a stateful source, or pushing into a stateful sink modifies
--   the internal state.
data FS


-------------------------------------------------------------------------------
data Size
        = -- | Flow produces exactly this number of elements.
          Exact Int#

          -- | Flow produces at most this number of elements.
        | Max   Int#


-- | Take the minimum of two flow sizes.
sizeMin :: Size -> Size -> Size
sizeMin !s1 !s2
 = case (s1, s2) of
        (Exact len1, Exact len2)        -> Exact (min# len1 len2)
        (Exact len1, Max   len2)        -> Max   (min# len1 len2)
        (Max   len1, Exact len2)        -> Max   (min# len1 len2)
        (Max   len1, Max   len2)        -> Max   (min# len1 len2)
 where
        min# x1 x2 
         = if x1 <=# x2 then x1 else x2
        {-# INLINE min# #-}

{-# INLINE [0] sizeMin #-}

