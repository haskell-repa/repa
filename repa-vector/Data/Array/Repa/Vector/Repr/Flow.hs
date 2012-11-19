
module Data.Array.Repa.Vector.Repr.Flow
        ( O
        , Array (..)

        -- * Modes
        , FD, FS

        -- * Distributions
        , BB, BN

        -- * Conversions
        , toFlow, fromFlowP
        , flow,   unflowP

        -- * Segmented operations.
        , Segd
        , replicatesP
        , replicatesSplitP)
where
import Data.Array.Repa                          as R
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Flow.Par.Segd            (Segd, SplitSegd)
import Data.Array.Repa.Flow.Par.Distro          (BB, BN)
import Data.Array.Repa.Flow.Seq                 (FD, FS, Touch)
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import GHC.Exts


-- | A parallel flow with the given mode and distribution.
data O mode dist

instance U.Unbox a => Source (O mode dist) a where
 data Array (O mode dist) sh a
        =  forall r
        .  Source r a
        => AFlow
                !sh                   -- Overall extent of vector.
                (F.Flow mode dist a)  -- A delayed parallel flow.
                (Vector r a)          -- A LAZY cache of the computed elements.

 extent (AFlow sh _ _)
        = sh
 {-# INLINE [4] extent #-}

 linearIndex (AFlow _ _ vec) ix
        = unsafeLinearIndex vec ix
 {-# INLINE [4] linearIndex #-}

 deepSeqArray (AFlow _ _ vec) x
  = vec `seq` x
 {-# INLINE [4] deepSeqArray #-}


-------------------------------------------------------------------------------
-- | Convert a vector to a balanced flow.
flow :: Source r a => Vector r a -> Vector (O FD BB) a
flow vec
 = let  !(Z :. (I# len)) = extent vec
        get ix           = unsafeLinearIndex vec (I# ix)
   in   AFlow   (extent vec)
                (F.generate len get)
                vec
{-# INLINE [4] flow #-}


-- | Compute a delayed flow in parallel, producing an unboxed vector.
unflowP  :: (Touch a, U.Unbox a, F.Unflow dist) 
        => Vector (O FD dist) a -> Vector U a
unflowP (AFlow sh ff _)
        = AUnboxed sh (F.unflow ff)
{-# INLINE [4] unflowP #-}


-------------------------------------------------------------------------------
-- | Unpack a vector to a raw flow.
toFlow  :: Vector (O mode dist) a -> F.Flow mode dist a
toFlow (AFlow _ ff _)  = ff
{-# INLINE [4] toFlow #-}


-- | Convert a raw delayed flow to a vector.
--
--   The result contains a lazy application of unflow whose result will be
--   demanded if the vector is consumed by indexing.
fromFlowP 
        :: (U.Unbox a, Touch a, F.Unflow dist)
        => F.Flow FD dist a 
        -> Vector (O FD dist) a

fromFlowP ff
 = let  vec     = F.unflow ff
        len     = U.length vec
   in   AFlow   (Z :. len) 
                ff 
                (fromUnboxed (Z :.len) vec)
{-# INLINE [4] fromFlowP #-}


-------------------------------------------------------------------------------
-- | Segmented replicate.
replicatesP 
        :: (U.Unbox a, Touch a, Source r a)
        => Segd 
        -> Vector r a
        -> Vector (O FD BB) a

replicatesP segd vec
 = let  get ix  = unsafeLinearIndex vec (I# ix)
   in   fromFlowP (F.replicates segd get)
{-# INLINE [4] replicatesP #-}


-- | Segmented replicate that takes a pre-split segment descriptor.
replicatesSplitP 
        :: (U.Unbox a, Touch a, Source r a)
        => SplitSegd 
        -> Vector r a
        -> Vector (O FD BB) a

replicatesSplitP segd vec
 = let  get ix  = unsafeLinearIndex vec (I# ix)
   in   fromFlowP (F.replicatesSplit segd get)
{-# INLINE [4] replicatesSplitP #-}
