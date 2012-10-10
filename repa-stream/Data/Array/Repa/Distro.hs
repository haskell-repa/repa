
module Data.Array.Repa.Distro
        ( Distro (..)
        , balanced
        , fragStart
        , fragLength)
where
import GHC.Exts
 
-- | Describes the size of a vector, and how the elements are distributed
--   across the gang.
--
--   We have separate 'distroFragLength' and 'distroFragStart' functions
--   for performance reasons, and they must give coherent results, else undefined.
--
data Distro
        = Distro
        { -- | Total length of chain.
          distroLength          :: Int#

          -- | Number of fragments the chain is split into.
        , distroFrags           :: Int#

          -- | Get the length of a fragment.
        , distroFragLength      :: Int# -> Int#

          -- | Get where a fragment starts in the result.
        , distroFragStart       :: Int# -> Int# }


-- | Create a balanced `Distro`.
balanced 
        :: Int#         -- ^ Total length of result.
        -> Int#         -- ^ Number of fragments.
        -> Distro

balanced len frags
        = Distro
        { distroLength          = len
        , distroFrags           = frags
        , distroFragLength      = fragLength len frags
        , distroFragStart       = fragStart  len frags }


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
