
module Data.Array.Repa.Flow.Par.Generate
        ( generate
        , replicate
        , replicates
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
generate :: Int# -> (Int# -> a) -> Flow r BB a
generate len get
 = Flow distro frag
 where
        !(I# threads)    = Gang.gangSize Gang.theGang
        !distro          = balanced len threads

        frag n
         = let  !len'    = distroBalancedFragLength distro n
                !start'  = distroBalancedFragStart  distro n

                get' ix  = get (start' +# ix)
           in   Seq.generate len' get'
        {-# INLINE frag #-}
{-# INLINE [1] generate #-}


-------------------------------------------------------------------------------
-- | Produce an flow of the given length with the same value in each position.
replicate :: forall a r. Int# -> a -> Flow r BB a
replicate n x
        = generate n get
        where   get :: Int# -> a
                get _ = x
                {-# INLINE get #-}
{-# INLINE [1] replicate #-}


-------------------------------------------------------------------------------
-- | Segmented replicate, where we have a function that produces the value
--   to use for each segment.
replicates
        :: Segd
        -> (Int# -> a)
        -> Flow r BB a

replicates segd getSegVal
 = replicatesSplit (Segd.splitSegd segd) getSegVal
{-# INLINE [1] replicates #-}


-- | Segmented replicate, where we have a function that produces the value
--   to use for each segment.
--
--   This version takes a pre-split segment descriptor.
replicatesSplit
        :: SplitSegd
        -> (Int# -> a)
        -> Flow r BB a

replicatesSplit segd getSegVal
 = Flow distro frag
 where
        !distro = Segd.distroOfSplitSegd segd
        
        frag n  
         = let  chunk            = V.unsafeIndex (Segd.splitChunk segd) (I# n)
                !elems           = Segd.chunkElems chunk
                !segStart        = Segd.chunkStart chunk
                getSegLen'  seg  = let !(I# r) = U.unsafeIndex (Segd.chunkLengths chunk) (I# seg) in r
                getSegVal'  seg  = getSegVal (seg +# segStart)
           in   Seq.replicatesDirect elems getSegLen' getSegVal'
{-# INLINE [1] replicatesSplit #-}


-------------------------------------------------------------------------------
-- | Yield a vector of the given length containing values @x@, @x+1@ etc.
enumFromN :: Int# -> Int# -> Flow r BB Int
enumFromN first len
 = Flow distro frag
 where
        !(I# threads)   = Gang.gangSize Gang.theGang
        !distro         = balanced len threads

        frag n
         = let  !len'   = distroBalancedFragLength distro n
                !start' = distroBalancedFragStart  distro n +# first
           in   Seq.enumFromN start' len'
