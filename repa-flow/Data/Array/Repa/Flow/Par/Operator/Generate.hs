
module Data.Array.Repa.Flow.Par.Operator.Generate
        ( generate
        , replicate
        , replicates
        , replicatesSplit
        , enumFromN)
where
import Data.Array.Repa.Bulk.Gang
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Par.Flow
import Data.Array.Repa.Flow.Par.Distro
import Data.Array.Repa.Flow.Par.Segd                        (Segd, SplitSegd)
import qualified Data.Array.Repa.Flow.Par.Segd              as Segd
import qualified Data.Array.Repa.Flow.Seq.Operator.Generate as Seq
import GHC.Exts
import Prelude hiding (replicate)


-------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying a function to each index.
generate :: Elt a => Gang -> Int# -> (Int# -> a) -> Flow FD BB a
generate gang len get
 = Flow gang distro start frag
 where
        !threads        = gangSize gang
        !distro         = balanced threads len

        start
         = return ()

        frag _ n
         = let  !len'    = distroBalancedFragLength distro n
                !start'  = distroBalancedFragStart  distro n

                get' ix  = get (start' +# ix)
           in   Seq.generate_i len' get'
        {-# INLINE frag #-}
{-# INLINE [2] generate #-}


-------------------------------------------------------------------------------
-- | Produce an flow of the given length with the same value in each position.
replicate :: Elt a => Gang -> Int# -> a -> Flow FD BB a
replicate gang n x
        = generate gang n (\_ -> x)
{-# INLINE [2] replicate #-}


-------------------------------------------------------------------------------
-- | Segmented replicate, where we have a function that produces the value
--   to use for each segment.
replicates
        :: Gang
        -> Segd
        -> (Int# -> a)
        -> Flow FD BB a

replicates gang segd getSegVal
 = replicatesSplit (Segd.splitSegd gang segd) getSegVal
{-# INLINE [2] replicates #-}


-- | Segmented replicate, where we have a function that produces the value
--   to use for each segment.
--
--   This version takes a pre-split segment descriptor.
replicatesSplit
        :: SplitSegd
        -> (Int# -> a)
        -> Flow FD BB a

replicatesSplit segd getSegVal
 = Flow gang distro start replicatesSplit_frag
 where
        here            = "par.replicatesSplit"
        !gang           = Segd.splitGang segd
        !distro         = Segd.distroOfSplitSegd segd

        start           = return ()

        replicatesSplit_frag _ n  
         = let  chunk            = vindex here (Segd.splitChunks segd) (I# n)
                !csegd           = Segd.chunkSegd chunk
                !elems           = Segd.elements csegd
                !segStart        = Segd.chunkStart chunk
                getSegLen'  seg  = let !(I# r) = uindex here
                                                        (Segd.lengths csegd) 
                                                        (I# seg)
                                   in r
                getSegVal'  seg  = getSegVal (seg +# segStart)
           in   Seq.replicates_bi elems getSegLen' getSegVal'
        {-# INLINE replicatesSplit_frag #-}

{-# INLINE [2] replicatesSplit #-}


-------------------------------------------------------------------------------
-- | Yield a vector containing values @x@, @x+1@ etc.
enumFromN 
        :: Gang
        -> Int#                 -- ^ Starting value.
        -> Int#                 -- ^ Length of result.
        -> Flow FD BB Int

enumFromN gang first len
 = Flow gang distro start frag
 where
        !threads        = gangSize gang
        !distro         = balanced threads len

        start
         = return ()

        frag _ n
         = let  !len'   = distroBalancedFragLength distro n
                !start' = distroBalancedFragStart  distro n +# first
           in   Seq.enumFromN_i start' len'
        {-# INLINE frag #-}
{-# INLINE [2] enumFromN #-}

