
module Data.Array.Repa.Flow.Seq.Base
        ( FD, FS
        , Size(..)
        , sizeMin)
where
import GHC.Exts


-------------------------------------------------------------------------------
-- | Phantom type tag to indicate a delayed flow.
--
--   A delayed flow is a flow that hasn't started flowing yet.
--   It does not have any attached state, and we can evaluate it multiple
--   times to get the same result.
--
data FD


-- | Phantom type tag to indicate a stateful flow.
--      
--   A stateful flow is a flow that has already started flowing.
--   It has attached state, and we can evaluate prefixes of it
--   incrementally. Evaluating the whole flow uses it up, 
--   so we can't evaluate it again.
--
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

