
module Data.Array.Repa.Flow.Seq.Base
        ( FD, FS
        , Size          (..)
        , sizeMin)
where
import GHC.Exts


-------------------------------------------------------------------------------
-- | Type index to indicate a delayed flow or co-flow. 
--   Being delayed means the (co)flow does not yet have any attached state.
--
--   A delayed flow may be attached to manifest source vectors, but it
--   it does not yet have indices into the source. We can evaluate a delayed
--   flow multiple times and get the same result.
--
--   A delayed coflow has not yet initialised its output buffer.
--   We can multiple flows into a coflow and construct the same output.
--
data FD


-- | Type index to indicate a stateful flow or co-flow.
--   
--   Pulling from a stateful flow, or pushing into a stateful co-flow modifies
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

