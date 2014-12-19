
module Data.Array.Repa.Repr.Unsafe.Unboxed
        ( UU, U.Unbox
        , Array (..)
        , fromListUU
        , fromVectorUU
        , toVectorUU)
where
import Data.Array.Repa.Bulk
import Data.Array.Repa.Shape
import Data.Array.Repa.Repr.Delayed
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad


---------------------------------------------------------------------------------------------------
-- | Unboxed arrays are represented as unsafe unboxed vectors.
--
--   UNSAFE: Indexing into this array is not bounds checked.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
data UU

-- | Read elements from an unboxed vector array.
instance (Shape sh, U.Unbox a) => Bulk UU sh a where
 data Array UU sh a
        = UUArray !sh !(U.Vector a)

 linearIndex (UUArray _ vec) ix
        = vec `U.unsafeIndex` ix
 {-# INLINE linearIndex #-}

 extent (UUArray sh _)
        = sh
 {-# INLINE extent #-}

 slice = error "UU slice not finished"
 {-# NOINLINE slice #-}


deriving instance (Show sh, Show e, U.Unbox e)
        => Show (Array UU sh e)

deriving instance (Read sh, Read e, U.Unbox e)
        => Read (Array UU sh e)


-- Target -----------------------------------------------------------------------------------------
instance U.Unbox e => Target UU e where
 data Buffer UU e 
  = UUBuffer (UM.IOVector e)

 unsafeNewBuffer len
  = liftM UUBuffer (UM.unsafeNew len)
 {-# INLINE unsafeNewBuffer #-}

 unsafeWriteBuffer (UUBuffer mvec) ix
  = UM.unsafeWrite mvec ix
 {-# INLINE unsafeWriteBuffer #-}

 unsafeFreezeBuffer sh (UUBuffer mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  UUArray sh vec
 {-# INLINE unsafeFreezeBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE touchBuffer #-}


-- Conversions ------------------------------------------------------------------------------------
-- | O(n). Convert a list to an unboxed vector array.
-- 
--   * This is an alias for `fromList` with a more specific type.
--
fromListUU
        :: (Shape sh, U.Unbox a)
        => sh -> [a] -> Maybe (Array UU sh a)
fromListUU = fromList
{-# INLINE [1] fromListUU #-}


-- | O(1). Wrap an unboxed vector as an array.
fromVectorUU
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array UU sh e
fromVectorUU sh vec
        = UUArray sh vec
{-# INLINE [1] fromVectorUU #-}


-- | O(1). Unpack an unboxed vector from an array.
toVectorUU
        :: U.Unbox e
        => Array UU sh e -> U.Vector e
toVectorUU (UUArray _ vec)
        = vec
{-# INLINE [1] toVectorUU #-}
