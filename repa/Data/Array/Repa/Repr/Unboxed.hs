
module Data.Array.Repa.Repr.Unboxed
        ( U, U.Unbox, Array (..)
        , computeUnboxed,  fromListUnboxed
        , fromUnboxed, toUnboxed)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Delayed
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad

-- | Unboxed arrays are represented as unboxed vectors.
--   The implementation of `Data.Vector.Unboxed` is based on type families and
--   picks an efficient, specialised representation for every element type. In
--   particular, unboxed vectors of pairs are represented as pairs of unboxed
--   vectors. This is the most efficient representation for numerical data.
data U
data instance U.Unbox e => Array U sh e
        = AUnboxed sh !(U.Vector e)
        
deriving instance (Show sh, Show e, U.Unbox e)
        => Show (Array U sh e)

-- Repr -----------------------------------------------------------------------
-- | Read elements from an unboxed vector array.
instance U.Unbox a => Repr U a where
 {-# INLINE linearIndex #-}
 linearIndex (AUnboxed _ vec) ix
        = vec U.! ix

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex (AUnboxed _ vec) ix
        = vec `U.unsafeIndex` ix

 {-# INLINE extent #-}
 extent (AUnboxed sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AUnboxed sh vec) x 
  = sh `deepSeq` vec `seq` x


-- Fill -----------------------------------------------------------------------
-- | Filling of unboxed vector arrays.
instance U.Unbox e => Fillable U e where
 data MArr U e 
  = UMArr (UM.IOVector e)

 {-# INLINE newMArr #-}
 newMArr n
  = liftM UMArr (UM.new n)

 {-# INLINE unsafeWriteMArr #-}
 unsafeWriteMArr (UMArr v) ix
  = UM.unsafeWrite v ix

 {-# INLINE unsafeFreezeMArr #-}
 unsafeFreezeMArr sh (UMArr mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  AUnboxed sh vec


-- Conversions ----------------------------------------------------------------
-- | Compute array elements in parallel.
--
--   * This is an alias for `compute` with a more specific type.
--
computeUnboxed
        :: Fill r1 U sh e
        => Array r1 sh e -> Array U sh e
{-# INLINE computeUnboxed #-}
computeUnboxed = compute


-- | O(n). Convert a list to an unboxed vector array.
-- 
--   * This is an alias for `fromList` with a more specific type.
--
fromListUnboxed
        :: (Shape sh, U.Unbox a)
        => sh -> [a] -> Array U sh a
{-# INLINE fromListUnboxed #-}
fromListUnboxed = fromList


-- | O(1). Wrap an unboxed vector as an array.
fromUnboxed
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U sh e
{-# INLINE fromUnboxed #-}
fromUnboxed sh vec
        = AUnboxed sh vec


-- | O(1). Unpack an unboxed vector from an array.
toUnboxed
        :: U.Unbox e
        => Array U sh e -> U.Vector e
{-# INLINE toUnboxed #-}
toUnboxed (AUnboxed _ vec)
        = vec
