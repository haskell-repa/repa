
module Data.Array.Repa.Repr.Unboxed
        ( U, Array (..)
        , fromUnboxed, toUnboxed
        , force)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad

-- | Unboxed arrays are represented as unboxed vectors.
--   The implementation of `Data.Vector.Unboxed` is based on type families and
--   picks an efficient, specialised representation for every element type. In
--   particular, unboxed vectors of pairs are represented as pairs of unboxed
--   vectors.
data U
data instance Array U sh e
        = AUnboxed sh !(U.Vector e)
        
deriving instance (Show sh, Show e, U.Unbox e)
        => Show (Array U sh e)

-- Repr -----------------------------------------------------------------------
-- | Use elements from an unboxed vector array.
instance U.Unbox a => Repr U a where
 {-# INLINE linearIndex #-}
 linearIndex (AUnboxed sh  vec) ix
        = vec U.! ix

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex (AUnboxed sh vec) ix
        = vec `U.unsafeIndex` ix

 {-# INLINE extent #-}
 extent (AUnboxed sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AUnboxed sh vec) x 
  = sh `deepSeq` vec `seq` x


-- Fill -----------------------------------------------------------------------
-- | Filling of unboxed vector arrays.
instance (Elt e, U.Unbox e) => Fillable U e where
 data MArr U e 
  = UMArr (UM.IOVector e)

 {-# INLINE newMArr #-}
 newMArr n
  = liftM UMArr (UM.new n)

 {-# INLINE writeMArr #-}
 writeMArr (UMArr v) ix
  = UM.write v ix

 {-# INLINE unsafeFreezeMArr #-}
 unsafeFreezeMArr sh (UMArr mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  AUnboxed sh vec


-- Load -----------------------------------------------------------------------
-- | no-op.
instance Shape sh => Load U U sh e where
 {-# INLINE load #-}
 load arr = arr

-- | O(n) construction of unboxed vector array.
instance (Shape sh, U.Unbox e) => Load L U sh e where
 {-# INLINE load #-}
 load (AList sh xx)
        = AUnboxed sh $ U.fromList xx

-- | O(n) construction of list.
instance (Shape sh, U.Unbox e) => Load U L sh e where
 {-# INLINE load #-}
 load (AUnboxed sh vec)
        = AList sh $ U.toList vec


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromUnboxed
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U sh e
{-# INLINE fromUnboxed #-}
fromUnboxed sh vec
        = AUnboxed sh vec


-- | Convert an array to a flat unboxed vector.
-- 
--   This is O(1) if the source is already represented as an unboxed vector (has representation `U`).
--
toUnboxed
        :: (Load r1 U sh e, U.Unbox e) 
        => Array r1 sh e -> U.Vector e
{-# INLINE toUnboxed #-}
toUnboxed arr
 = case load arr of
        AUnboxed _ vec  -> vec


-- | Force an array to an unboxed vector.
--
--   If the source array was in delayed form then this invokes parallel computation.
--
force   :: (Load r1 U sh e)
        => Array r1 sh e -> Array U sh e
{-# INLINE force #-}
force = load
