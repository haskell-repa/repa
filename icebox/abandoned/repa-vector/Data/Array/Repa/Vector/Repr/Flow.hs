
module Data.Array.Repa.Vector.Repr.Flow
        ( O
        , DistShape (..)
        , Array (..)

        -- * Modes
        , FD, FS

        -- * Distributions
        , BB, BN

        -- * Conversions
        , flow
        , toFlow
        , fromFlowBB

        -- * Computation
        , Unflow (..))
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Bulk.Gang
import Data.Array.Repa.Vector.Base                      as R
import Data.Array.Repa.Vector.Compute                   as R
import Data.Array.Repa.Vector.Repr.Unboxed              as R
import Data.Array.Repa.Vector.Operators.Bulk            as R
import Data.Array.Repa.Flow.Par.Distro                  (BB, BN)
import Data.Array.Repa.Flow.Seq                         (FD, FS)
import qualified Data.Array.Repa.Flow.Par.Distro        as FP
import qualified Data.Array.Repa.Flow.Par               as FP
import qualified Data.Array.Repa.Flow.Seq               as FS
import qualified Data.Vector.Unboxed                    as U
import GHC.Exts


-- | A parallel flow with the given mode and distribution.
data O mode dist

data instance Array (O mode dist) sh a
        = AFlow (DistShape dist sh) (FP.Flow mode dist a)

data family DistShape dist sh
data instance DistShape BB sh   = DistBB sh
data instance DistShape BN sh   = DistBN


-------------------------------------------------------------------------------
-- | Convert a bulk array  to a balanced flow.
flow    :: (Shape sh, Bulk r a)
        => Array r sh a -> Array (O FD BB) sh a
flow vec
 = AFlow (DistBB $ extent vec) (FP.generate theGang len get)
 where  !(I# len)       = R.size $ R.extent vec
        get ix          = linearIndex vec (I# ix)
{-# INLINE [4] flow #-}

-------------------------------------------------------------------------------
-- | Unpack a vector to a raw flow.
toFlow  :: Vector (O mode dist) a -> FP.Flow mode dist a
toFlow (AFlow _ ff) = ff
{-# INLINE [4] toFlow #-}


-- | Pack a raw flow into a vector.
fromFlowBB :: sh -> FP.Flow mode BB a -> Array (O mode BB) sh a
fromFlowBB sh ff       = AFlow (DistBB sh) ff
{-# INLINE [4] fromFlowBB #-}


------------------------------------------------------------------------------
class Unflow dist sh where
 -- | Compute a delayed flow in parallel.
 -- 
 --   Computing an unbalanced flow must produce a 1D vector rather than
 --   a more general array because the number of elements that will be 
 --   produced is not known up-front.
 unflowP :: (Shape sh, U.Unbox a, Elt a)
         => Array (O FD dist) sh a -> Array U sh a


-- | Compute a delayed, balanced flow.
instance (Shape sh, Elt a) => Compute (O FD BB) sh a where

 -- Parallel version.
 computeIOP (AFlow (DistBB sh) 
                   (FP.Flow gang distro startPar frag))
  = do
        let !len          = FP.distroBalancedLength    distro
        let !getFragStart = FP.distroBalancedFragStart distro

        -- Allocate a mutable vector to hold the result.
        !mvec     <- newMVec (I# len)

        -- Start the parallel flow.
        !statePar <- startPar

        -- The action that runs on each thread.
        let action tid
             = case frag statePar tid of
                FS.Flow startSeq _size _reportSeq get1 get8
                 -> do  
                        -- The starting point for this thread's result
                        -- in the final vector.
                        let !ixStart    = getFragStart tid

                        -- Start this sequential flow fragment.
                        !stateSeq       <- startSeq

                        -- Slurp element from the flow fragment into
                        -- the result vector.
                        let write ix val
                                = unsafeWriteMVec mvec (I# (ixStart +# ix)) val

                        _     <- FS.slurp 
                                        0# Nothing
                                        write
                                        (get1 stateSeq) (get8 stateSeq)

                        return ()

        -- Run the actions that evaluate each fragment in parallel.
        gangIO gang action

        -- Freeze the mutable vector into a Repa array.
        unsafeFreezeMVec sh mvec 


 -- Sequential version.
 -- This is the same as the parallel version, 
 -- except that we evaluate each flow fragment in the main thread.
 computeIOS (AFlow (DistBB sh) 
                   (FP.Flow gang distro startPar frag))
  = do
        let !len          = FP.distroBalancedLength    distro
        let !getFragStart = FP.distroBalancedFragStart distro

        -- Allocate a mutable vector to hold the result.
        !mvec     <- newMVec (I# len)

        -- Start the parallel flow.
        !statePar <- startPar

        -- The action that runs on each thread.
        let {-# INLINE action #-}
            action tid
             = case frag statePar tid of
                FS.Flow startSeq _size _reportSeq get1 get8
                 -> do  
                        -- The starting point for this thread's result
                        -- in the final vector.
                        let !ixStart    = getFragStart tid

                        -- Start this sequential flow fragment.
                        !stateSeq       <- startSeq

                        -- Slurp element from the flow fragment into
                        -- the result vector.
                        let write ix val
                                = unsafeWriteMVec mvec (I# (ixStart +# ix)) val

                        _     <- FS.slurp 
                                        0# Nothing
                                        write
                                        (get1 stateSeq) (get8 stateSeq)

                        return ()

        let runActions tid
             | tid >=# gangSize gang 
             = return ()

             | otherwise            
             = do action tid
                  runActions (tid +# 1#)

        runActions 0#

        -- Freeze the mutable vector into a Repa array.
        unsafeFreezeMVec sh mvec 


 {-# INLINE [4] computeIOP #-}


instance Shape sh => Unflow BB sh where
 -- | Compute a balanced, delayed flow in parallel, producing an unboxed vector.
 unflowP (AFlow (DistBB sh) ff)
  = let  !vec    = FP.unflow ff
    in   fromUnboxed sh vec
 {-# INLINE [4] unflowP #-}


instance Unflow BN DIM1 where
 unflowP (AFlow _ ff)
  = let  !vec    = FP.unflow ff
    in   fromUnboxed (Z :. U.length vec) vec
 {-# INLINE [4] unflowP #-}
