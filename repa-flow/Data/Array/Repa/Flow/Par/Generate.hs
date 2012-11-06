
module Data.Array.Repa.Flow.Par.Generate
        ( generate
        , replicate
        , replicates
        , replicatesUnboxed
        , replicatesSplit
        , enumFromN)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Distro
import Data.Array.Repa.Flow.Par.Segd                    (Segd, SplitSegd)
import qualified Data.Array.Repa.Flow.Par.Segd          as Segd
import qualified Data.Array.Repa.Flow.Seq.Generate      as Seq
import qualified Data.Array.Repa.Eval.Gang              as Gang
import qualified Data.Vector                            as V
import qualified Data.Vector.Unboxed                    as U
import GHC.Exts
import Prelude hiding (replicate)


-------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying a function to each index.
generate :: Int# -> (Int# -> a) -> Flow rep BB a
generate len get
 = Flow distro frag
 where
        !(I# threads)    = Gang.gangSize Gang.theGang
        !distro          = balanced threads len

        frag n
         = let  !len'    = distroBalancedFragLength distro n
                !start'  = distroBalancedFragStart  distro n

                get' ix  = get (start' +# ix)
           in   Seq.generate len' get'
        {-# INLINE frag #-}
{-# INLINE [2] generate #-}


-------------------------------------------------------------------------------
-- | Produce an flow of the given length with the same value in each position.
replicate :: forall a rep. Int# -> a -> Flow rep BB a
replicate n x
        = generate n get
        where   get :: Int# -> a
                get _ = x
                {-# INLINE get #-}
{-# INLINE [2] replicate #-}


-------------------------------------------------------------------------------
-- | Segmented replicate, where we have a function that produces the value
--   to use for each segment.
replicates
        :: Segd
        -> (Int# -> a)
        -> Flow rep BB a

replicates segd getSegVal
 = replicatesSplit (Segd.splitSegd segd) getSegVal
{-# INLINE [2] replicates #-}


-- | Segmented replicate, taking an unboxed vector of elemements.
replicatesUnboxed
        :: U.Unbox a 
        => Segd
        -> U.Vector a
        -> Flow rep BB a

replicatesUnboxed segd vec
 = replicates segd get
 where  get ix  = U.unsafeIndex vec (I# ix)
{-# INLINE replicatesUnboxed #-}


-- | Segmented replicate, where we have a function that produces the value
--   to use for each segment.
--
--   This version takes a pre-split segment descriptor.
replicatesSplit
        :: SplitSegd
        -> (Int# -> a)
        -> Flow rep BB a

replicatesSplit segd getSegVal
 = Flow distro frag
 where
        !distro = Segd.distroOfSplitSegd segd
        
        frag n  
         = let  chunk            = V.unsafeIndex (Segd.splitChunk segd) (I# n)
                !elems           = Segd.chunkElems chunk
                !segStart        = Segd.chunkStart chunk
                getSegLen'  seg  = let !(I# r) = U.unsafeIndex 
                                                        (Segd.chunkLengths chunk) 
                                                        (I# seg)
                                   in r
                getSegVal'  seg  = getSegVal (seg +# segStart)
           in   Seq.replicatesDirect elems getSegLen' getSegVal'
{-# INLINE [2] replicatesSplit #-}


-------------------------------------------------------------------------------
-- | Yield a vector containing values @x@, @x+1@ etc.
enumFromN 
        :: Int#                 -- ^ Starting value.
        -> Int#                 -- ^ Length of result.
        -> Flow rep BB Int

enumFromN first len
 = Flow distro frag
 where
        !(I# threads)   = Gang.gangSize Gang.theGang
        !distro         = balanced threads len

        frag n
         = let  !len'   = distroBalancedFragLength distro n
                !start' = distroBalancedFragStart  distro n +# first
           in   Seq.enumFromN start' len'
{-# INLINE [2] enumFromN #-}

