
module Data.Array.Repa.Vector.Repr.Unboxed
        ( U, U.Unbox, Array(..)
        , fromUnboxed
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


instance (Elt a, U.Unbox a) => Bulk U a where
 linearIndex (AUnboxed _ vec) ix
        -- Return a zero value for out-of-range indices.
        | ix >= U.length vec
        = zero

        -- This is actually safe because we just checked the bounds.
        | otherwise           
        = vec `U.unsafeIndex` ix
 {-# INLINE linearIndex #-}

 extent (AUnboxed sh _)
        = sh
 {-# INLINE extent #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
--   TODO: make this function safe, have an unsafeFromUnboxed that doesn't do 
--         the check. Put the vector behind a function that checks the bounds.
fromUnboxed
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U sh e
fromUnboxed sh vec
        = AUnboxed sh vec
{-# INLINE fromUnboxed #-}


-- | O(1). Unpack an unboxed vector from an array.
toUnboxed
        :: U.Unbox e
        => Array U sh e -> U.Vector e
toUnboxed (AUnboxed _ vec)
        = vec
{-# INLINE toUnboxed #-}
