
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
import Data.Array.Repa.Vector.Base              as R
import Data.Array.Repa.Vector.Repr.Unboxed      as R
import Data.Array.Repa.Vector.Operators.Bulk    as R
import Data.Array.Repa.Flow.Par.Distro          (BB, BN)
import Data.Array.Repa.Flow.Seq                 (FD, FS)
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import GHC.Exts


-- | A parallel flow with the given mode and distribution.
data O mode dist

data instance Array (O mode dist) sh a
        = AFlow (DistShape dist sh) (F.Flow mode dist a)

data family DistShape dist sh
data instance DistShape BB sh   = DistBB sh
data instance DistShape BN sh   = DistBN


-------------------------------------------------------------------------------
-- | Convert a bulk array  to a balanced flow.
flow    :: (Shape sh, Bulk r a)
        => Array r sh a -> Array (O FD BB) sh a
flow vec
 = AFlow (DistBB $ extent vec) (F.generate theGang len get)
 where  !(I# len)       = R.size $ R.extent vec
        get ix          = linearIndex vec (I# ix)
{-# INLINE [4] flow #-}

-------------------------------------------------------------------------------
-- | Unpack a vector to a raw flow.
toFlow  :: Vector (O mode dist) a -> F.Flow mode dist a
toFlow (AFlow _ ff) = ff
{-# INLINE [4] toFlow #-}


-- | Pack a raw flow into a vector.
fromFlowBB :: sh -> F.Flow mode BB a -> Array (O mode BB) sh a
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


-- instance Shape sh => Compute (O FD dist) sh a where
-- computeIOP (AFlow (DistBB sh) ff)
--  = let 


instance Shape sh => Unflow BB sh where
 -- | Compute a balanced, delayed flow in parallel, producing an unboxed vector.
 unflowP (AFlow (DistBB sh) ff)
  = let  !vec    = F.unflow ff
    in   fromUnboxed sh vec
 {-# INLINE [4] unflowP #-}


instance Unflow BN DIM1 where
 unflowP (AFlow _ ff)
  = let  !vec    = F.unflow ff
    in   fromUnboxed (Z :. U.length vec) vec
 {-# INLINE [4] unflowP #-}
