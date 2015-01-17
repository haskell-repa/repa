
module Data.Repa.Array.Material.Safe.Unboxed
        ( U(..), U.Unbox
        , Array (..),  unboxed
        , fromVectorU, toVectorU)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Eval.Array
import Data.Repa.Fusion.Unpack
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad
import Data.Word


-------------------------------------------------------------------------------
-- | Unboxed arrays are represented as unboxed vectors.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
data U = U
instance Repr U where
 repr = U


instance (Shape sh, U.Unbox a) => Bulk U sh a where
 data Array U sh a    = UArray sh !(U.Vector a)
 extent (UArray sh _) = sh
 index  (UArray sh vec) ix
        | not $ inShapeRange zeroDim sh ix
        = error "repa-bulk.index[U] out of range"

        | otherwise
        = vec U.! (toIndex sh ix)

 {-# INLINE extent #-}
 {-# INLINE index  #-}
 {-# SPECIALIZE instance Bulk U DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk U DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk U DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk U DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk U DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk U DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk U DIM1 Word64  #-}


-- | Constrain an array to have an unboxed representation,
--   eg with @unboxed (compute arr)@
unboxed :: Array U sh a -> Array U sh a
unboxed = id
{-# INLINE unboxed #-}

deriving instance (Show sh, Show e, U.Unbox e) => Show (Array U sh e)


-- Window ---------------------------------------------------------------------
instance U.Unbox a => Window U DIM1 a where
 window (Z :. start) (Z :. len) (UArray _sh vec)
        = UArray (Z :. len) (U.slice start len vec)
 {-# INLINE window #-}
 {-# SPECIALIZE instance Window U DIM1 Int     #-}
 {-# SPECIALIZE instance Window U DIM1 Float   #-}
 {-# SPECIALIZE instance Window U DIM1 Double  #-}
 {-# SPECIALIZE instance Window U DIM1 Word8   #-}
 {-# SPECIALIZE instance Window U DIM1 Word16  #-}
 {-# SPECIALIZE instance Window U DIM1 Word32  #-}
 {-# SPECIALIZE instance Window U DIM1 Word64  #-}


-- Target ---------------------------------------------------------------------
instance U.Unbox e => Target U e (UM.IOVector e) where
 data Buffer U e 
  = UBuffer !(UM.IOVector e)

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

 {-# SPECIALIZE instance Target U Int    (UM.IOVector Int)    #-}
 {-# SPECIALIZE instance Target U Float  (UM.IOVector Float)  #-}
 {-# SPECIALIZE instance Target U Double (UM.IOVector Double) #-}
 {-# SPECIALIZE instance Target U Word8  (UM.IOVector Word8)  #-}
 {-# SPECIALIZE instance Target U Word16 (UM.IOVector Word16) #-}
 {-# SPECIALIZE instance Target U Word32 (UM.IOVector Word32) #-}
 {-# SPECIALIZE instance Target U Word64 (UM.IOVector Word64) #-}


instance Unpack (Buffer U e) (UM.IOVector e) where
 unpack (UBuffer vec)  = vec
 repack _ vec          = UBuffer vec
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-- Conversions ----------------------------------------------------------------
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



