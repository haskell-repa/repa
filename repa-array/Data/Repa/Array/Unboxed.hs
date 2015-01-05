
module Data.Repa.Array.Unboxed
        ( U, U.Unbox
        , Array (..)
        , fromVectorU, toVectorU
        , unboxed)
where
import Data.Repa.Eval.Array
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Index
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad


-------------------------------------------------------------------------------
-- | Unboxed arrays are represented as unboxed vectors.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
data U

-- | Unboxed arrays.
instance (Shape sh, U.Unbox a) => Bulk U sh a where
 data Array U sh a
        = UArray sh !(U.Vector a)

 index  (UArray sh vec) ix
        | not $ inShapeRange zeroDim sh ix
        = error "repa-bulk.index[U] out of range"

        | otherwise
        = vec U.! (toIndex sh ix)

 extent (UArray sh _) = sh
 {-# INLINE extent #-}


deriving instance (Show sh, Show e, U.Unbox e) => Show (Array U sh e)
deriving instance (Read sh, Read e, U.Unbox e) => Read (Array U sh e)

-- | Constrain an array to have an unboxed representation,
--   eg with @unboxed (compute arr)@
unboxed :: Array U sh a -> Array U sh a
unboxed = id
{-# INLINE unboxed #-}


-- Window -----------------------------------------------------------------------------------------
instance U.Unbox a => Window U DIM1 a where
 window (Z :. start) (Z :. len) (UArray _sh vec)
        = UArray (Z :. len) (U.slice start len vec)
 {-# INLINE window #-}


-- Target -----------------------------------------------------------------------------------------
instance U.Unbox e => Target U e where
 data Buffer U e 
  = UBuffer (UM.IOVector e)

 unsafeNewBuffer len
  = liftM UBuffer (UM.unsafeNew len)
 {-# INLINE unsafeNewBuffer #-}

 unsafeWriteBuffer (UBuffer mvec) ix
  = UM.unsafeWrite mvec ix
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer (UBuffer mvec) bump
  = do  mvec'   <- UM.unsafeGrow mvec bump
        return  $ UBuffer mvec'
 {-# INLINE unsafeGrowBuffer #-}

 unsafeSliceBuffer start len (UBuffer mvec)
  = do  let mvec'  = UM.unsafeSlice start len mvec
        return  $  UBuffer mvec'
 {-# INLINE unsafeSliceBuffer #-}

 unsafeFreezeBuffer sh (UBuffer mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  UArray sh vec
 {-# INLINE unsafeFreezeBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE touchBuffer #-}


-- Conversions ------------------------------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromVectorU :: (Shape sh, U.Unbox e)
            => sh -> U.Vector e -> Array U sh e
fromVectorU sh vec
        = UArray sh vec
{-# INLINE [1] fromVectorU #-}


-- | O(1). Unpack an unboxed vector from an array.
toVectorU
        :: U.Unbox e
        => Array U sh e -> U.Vector e
toVectorU (UArray _ vec)
        = vec
{-# INLINE [1] toVectorU #-}

