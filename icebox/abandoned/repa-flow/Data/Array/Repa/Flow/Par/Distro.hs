
-- | Describes the size of a vector, and how the elements are distributed
--   across the gang.
--
--   We have separate 'distroFragLength' and 'distroFragStart' functions
--   for performance reasons, and they must give coherent results,
--   else undefined.
--
module Data.Array.Repa.Flow.Par.Distro
        ( BB, BN
        , Distro (..)

        -- * Balanced Distributions
        , balanced
        , balancedFragStart
        , balancedFragLength

        -- * Unbalanced Distributions
        , unbalanced)

where
import GHC.Exts


-- | Describes the distribution of a flow between several threads.
data family Distro d


-- Balanced -------------------------------------------------------------------
-- | Type index to indicate an balanced distribution.
data BB

data instance Distro BB
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
        :: Int#         -- ^ Number of fragments.
        -> Int#         -- ^ Total length of result.
        -> Distro BB

balanced frags len
        = DistroBalanced
        { distroBalancedLength          = len
        , distroBalancedFrags           = frags
        , distroBalancedFragLength      = balancedFragLength frags len
        , distroBalancedFragStart       = balancedFragStart  frags len }
{-# INLINE [1] balanced #-}


-- | Given the length of a vector, 
--   get the starting point for one fragment,
--   dividing it evenly between the threads.
balancedFragStart
        :: Int#         -- ^ Number of fragments.
        -> Int#         -- ^ Total length of result.
        -> Int#         -- ^ Fragment (thread) number
        -> Int#         -- ^ Starting point of this fragment.

balancedFragStart frags len i
 | frags ==# 0#   
 = error "Data.Array.Repa.Flow.Par.Distro.balancedFragStart: no fragments"

 | otherwise
 = let  fragLen         = len `quotInt#` frags
        fragLeftover    = len `remInt#`  frags

        getStart i'
         | i' <# fragLeftover   = i' *# (fragLen +# 1#)
         | otherwise            = i' *# fragLen  +# fragLeftover
        {-# NOINLINE getStart #-}

  in    getStart i
{-# NOINLINE balancedFragStart #-}
--  NOINLINE because it's only called when distributing work, 
--  not in inner loops, and we want to keep the code size down.


-- | Get the length of a vector,
--   get the starting point for one fragment,
--   dividing it evenly between the threads.
balancedFragLength 
        :: Int#         -- ^ Total length of result.
        -> Int#         -- ^ Number of fragments.
        -> Int#         -- ^ Fragment (thread) number.
        -> Int#         -- ^ Length of this fragment.

balancedFragLength frags len i
        =  balancedFragStart frags len (i +# 1#)
        -# balancedFragStart frags len i
{-# NOINLINE balancedFragLength #-}
--  NOINLINE because it's only called when distributing work, 
--  not in inner loops, and we want to keep the code size down.


-- Unbalanced -----------------------------------------------------------------
-- | Type index to indicate an unbalanced distribution.
data BN

data instance Distro BN
        = DistroUnbalanced
        { -- | Number of fragments the flow is split into.
          distroUnbalancedFrags :: Int# }
 
unbalanced 
        :: Int#         -- ^ Number of fragments.
        -> Distro BN

unbalanced frags
        = DistroUnbalanced
        { distroUnbalancedFrags = frags }
{-# INLINE [1] unbalanced #-}

