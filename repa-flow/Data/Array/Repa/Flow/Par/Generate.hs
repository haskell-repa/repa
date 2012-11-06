
module Data.Array.Repa.Flow.Par.Generate
        ( generate
        , replicate
        , replicatesDirectFragged
        , enumFromN)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Distro
import qualified Data.Array.Repa.Flow.Seq.Generate      as Seq
import qualified Data.Array.Repa.Eval.Gang              as Gang
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
-- | Segmented replicate, where we have functions that produce the segment
--   lengths and elements directly.
replicatesDirectFragged
        :: Distro BB
        -> (Int# -> Int# -> Int#) -- ^ FragId -> SegId -> Seg Length
        -> (Int# -> Int# -> a)    -- ^ FragId -> SegId -> Value to emit for this seg.
        -> Flow r BB a

replicatesDirectFragged !distro getSegLen getSegVal
 = Flow distro frag
 where
        frag n
         = let  !fragLen        = distroBalancedFragLength distro n
                !getSegLen'     = getSegLen n
                !getSegVal'     = getSegVal n
           in   Seq.replicatesDirect fragLen getSegLen' getSegVal'
{-# INLINE [1] replicatesDirectFragged #-}


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
