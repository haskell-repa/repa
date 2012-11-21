
module Data.Array.Repa.Vector.Repr.Flow
        ( O
        , Array (..)

        -- * Modes
        , FD, FS

        -- * Distributions
        , BB, BN

        -- * Conversions
        , flow,   unflowP
        , toFlow, fromFlow)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Operators.Bulk    as R
import Data.Array.Repa.Flow.Par.Distro          (BB, BN)
import Data.Array.Repa.Flow.Seq                 (FD, FS, Touch)
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import GHC.Exts


-- | A parallel flow with the given mode and distribution.
data O mode dist

data instance Array (O mode dist) DIM1 a
        = AFlow (F.Flow mode dist a)


-------------------------------------------------------------------------------
-- | Convert a bulk vector to a balanced flow.
flow ::  Bulk r a => Vector r a -> Vector (O mode BB) a
flow vec
 = AFlow (F.generate len get)
 where  !(I# len)       = R.length vec
        get ix          = linearIndex vec (I# ix)
{-# INLINE [4] flow #-}


-- | Compute a delayed flow in parallel, producing an unboxed vector.
unflowP :: (F.Unflow dist, U.Unbox a, Touch a)
        => Vector (O FD dist) a -> Vector U a
unflowP (AFlow ff)
 = let  !vec    = F.unflow ff
   in   fromUnboxed (Z :. U.length vec) vec
{-# INLINE [4] unflowP #-}


-------------------------------------------------------------------------------
-- | Unpack a vector to a raw flow.
toFlow  :: Vector (O mode dist) a -> F.Flow mode dist a
toFlow (AFlow ff) = ff
{-# INLINE [4] toFlow #-}


-- | Pack a raw flow into a vector.
fromFlow :: F.Flow mode dist a -> Vector (O mode dist) a
fromFlow ff       = AFlow ff
{-# INLINE [4] fromFlow #-}
