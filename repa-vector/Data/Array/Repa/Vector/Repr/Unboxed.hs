
module Data.Array.Repa.Vector.Repr.Unboxed
        ( U, U.Unbox, Array(..)

          -- * Conversions
        , fromUnboxed
        , toUnboxed

          -- * Unsafe conversions
        , unsafeFromUnboxed
        , release)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Bulk
import qualified Data.Vector.Unboxed              as U
import GHC.Exts

-- | Unboxed arrays are represented as unboxed vectors.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
data U

data instance Array U sh a
        = AUnboxed !sh !(U.Vector a) (Int# -> a)


instance (Elt a, U.Unbox a) => Bulk U a where
 linearIndex (AUnboxed _ _ get) (I# ix)
        = get ix
 {-# INLINE [4] linearIndex #-}

 extent (AUnboxed sh _ _)
        = sh
 {-# INLINE [4] extent #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
--   If the result is indexed outside its nominal range then `error`.
fromUnboxed
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U sh e

fromUnboxed sh vec
 = let  get ix  = vec U.! (I# ix)
   in   AUnboxed sh vec get
{-# INLINE [4] fromUnboxed #-}


-- | O(1). Unpack an unboxed vector from an array.
toUnboxed
        :: U.Unbox e
        => Array U sh e -> U.Vector e
toUnboxed (AUnboxed _ vec _)
        = vec
{-# INLINE [4] toUnboxed #-}


-- Unsafe conversions ---------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array, without bounds checks.
--
--   All operations that consume the result will be unsafe.
--   Execution sanity is not preserved if consumers index the array outside
--   of its nominal range.
unsafeFromUnboxed
        :: U.Unbox e
        => sh -> U.Vector e -> Array U sh e

unsafeFromUnboxed sh vec
 = let  get ix  = vec `U.unsafeIndex` (I# ix)
   in   AUnboxed sh vec get
{-# INLINE [4] unsafeFromUnboxed #-}


-- | O(1). Release an unboxed array from bounds checking.
--      
--   All operations that consume the result will be unsafe.
--   Execution sanity is not preserved if consumers index the array outside
--   of its nominal range.
release :: U.Unbox a => Array U sh a -> Array U sh a
release (AUnboxed sh vec _)
 = unsafeFromUnboxed sh vec
{-# INLINE [4] release #-}

