
-- | Describes the size of a vector, and how the elements are distributed
--   across the gang.
--
--   We have separate 'distroFragLength' and 'distroFragStart' functions
--   for performance reasons, and they must give coherent results,
--   else undefined.
--
module Data.Array.Repa.Flow.Par.Distro
        ( L, N
        , Distro (..)
        , balanced)
where
import GHC.Exts

-- | Describes the distribution of a flow between several threads.
data family Distro d

-- Balanced -------------------------------------------------------------------
-- | Type index to indicate an unbalanced distribution.
data L

data instance Distro L
        = DistroBalanced
        { -- | Number of fragments the flow is split into.
          distroBalancedFrags           :: Int#

          -- | Total length of flow.
        , distroBalancedLength          :: Int#

          -- | Get the length of a fragment.
        , distroBalancedFragLength      :: Int# -> Int#

          -- | Get where a fragment starts in the result.
        , distroBalancedFragStart       :: Int# -> Int# }


-- | Create a balanced `Distro`.
balanced 
        :: Int#         -- ^ Total length of result.
        -> Int#         -- ^ Number of fragments.
        -> Distro L

balanced len frags
        = DistroBalanced
        { distroBalancedLength          = len
        , distroBalancedFrags           = frags
        , distroBalancedFragLength      = fragLength len frags
        , distroBalancedFragStart       = fragStart  len frags }


-- | Given the length of a vector, 
--   get the starting point for one fragment,
--   dividing it evenly between the threads.
fragStart
        :: Int#         -- ^ Total length of result.
        -> Int#         -- ^ Number of fragments.
        -> Int#         -- ^ Fragment (thread) number
        -> Int#         -- ^ Starting point of this fragment.

fragStart len frags i
 = let  fragLen         = len `quotInt#` frags
        fragLeftover    = len `remInt#`  frags

        getStart i'
         | i' <# fragLeftover   = i' *# (fragLen +# 1#)
         | otherwise            = i' *# fragLen  +# fragLeftover
        {-# NOINLINE getStart #-}

  in    getStart i
{-# NOINLINE fragStart #-}
--  NOINLINE because it's only called when distributing work, 
--  not in inner loops, and we want to keep the code size down.


-- | Get the length of a vector,
--   get the starting point for one fragment,
--   dividing it evenly between the threads.
fragLength 
        :: Int#         -- ^ Total length of result.
        -> Int#         -- ^ Number of fragments.
        -> Int#         -- ^ Fragment (thread) number.
        -> Int#         -- ^ Length of this fragment.

fragLength len frags i
        =  fragStart len frags (i +# 1#)
        -# fragStart len frags i
{-# NOINLINE fragLength #-}
--  NOINLINE because it's only called when distributing work, 
--  not in inner loops, and we want to keep the code size down.


-- Unbalanced -----------------------------------------------------------------
-- | Type index to indicate a balanced distribution.
data N

data instance Distro N
        = DistroUnbalanced
        { -- | Number of fragments the flow is split into.
          distroFrags           :: Int# }
 
