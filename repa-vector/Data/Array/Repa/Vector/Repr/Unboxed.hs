
module Data.Array.Repa.Vector.Repr.Unboxed
        ( U, U.Unbox, Array(..)

          -- * Conversions
        , fromUnboxed
        , fromListUnboxed
        , toUnboxed)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Bulk
import qualified Data.Vector.Unboxed              as U


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
        = AUnboxed !sh !(U.Vector a)

deriving instance (U.Unbox a, Show sh, Show a) => Show (Array U sh a)
deriving instance (U.Unbox a, Read sh, Read a) => Read (Array U sh a)
 


instance (Elt a, U.Unbox a) => Bulk U a where
 linearIndex (AUnboxed _ vec) ix
        = U.unsafeIndex vec ix
 {-# INLINE [4] linearIndex #-}

 extent (AUnboxed sh _)
        = sh
 {-# INLINE [4] extent #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
--   If the result is indexed outside its nominal range then `error`.
fromUnboxed
        :: (Shape sh, U.Unbox a)
        => sh -> U.Vector a -> Array U sh a

fromUnboxed sh vec
        = AUnboxed sh vec
{-# INLINE [4] fromUnboxed #-}


-- | O(n). Convert a list to an unboxed array.
fromListUnboxed
        :: (Shape sh, U.Unbox a)
        => sh -> [a] -> Array U sh a

fromListUnboxed sh list
        = AUnboxed sh (U.fromList list)
{-# INLINE [4] fromListUnboxed #-}


-- | O(1). Unpack an unboxed vector from an array.
toUnboxed
        :: U.Unbox a
        => Array U sh a -> U.Vector a
toUnboxed (AUnboxed _ vec)
        = vec
{-# INLINE [4] toUnboxed #-}

