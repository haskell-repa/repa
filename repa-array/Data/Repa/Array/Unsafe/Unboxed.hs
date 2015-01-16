
module Data.Repa.Array.Unsafe.Unboxed
        ( UU(..), U.Unbox
        , Array (..),   uunboxed

        -- * Slicing
        , slicesUU

        -- * Conversions
        , fromVectorUU, toVectorUU)

where
import Data.Repa.Fusion.Unpack
import Data.Repa.Eval.Array
import Data.Repa.Array.Window
import Data.Repa.Array.Delayed
import Data.Repa.Array.Unsafe.Nested
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Index
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad
import Data.Word


-------------------------------------------------------------------------------
-- | Unboxed arrays are represented as unsafe unboxed vectors.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
--   UNSAFE: Indexing into this array is not bounds checked.
--
data UU = UU
instance Repr UU where
 repr = UU
 

instance (Shape sh, U.Unbox a) => Bulk UU sh a where
 data Array UU sh a        = UUArray !sh !(U.Vector a)
 extent (UUArray sh _)     = sh
 index (UUArray sh vec) ix = vec `U.unsafeIndex` (toIndex sh ix)
 {-# INLINE extent #-}
 {-# INLINE index #-}
 {-# SPECIALIZE instance Bulk UU DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk UU DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk UU DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk UU DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk UU DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk UU DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk UU DIM1 Word64  #-}

deriving instance (Show sh, Show e, U.Unbox e) => Show (Array UU sh e)


-- | Constrain an array to have an unsafe unboxed representation,
--   eg with @unboxed (compute arr)@
uunboxed :: Array UU sh a -> Array UU sh a
uunboxed = id
{-# INLINE uunboxed #-}


-- Window ---------------------------------------------------------------------
instance U.Unbox a => Window UU DIM1 a where
 window (Z :. start) (Z :. len) (UUArray _sh vec)
        = UUArray (Z :. len) (U.slice start len vec)
 {-# INLINE window #-}
 {-# SPECIALIZE instance Window UU DIM1 Int     #-}
 {-# SPECIALIZE instance Window UU DIM1 Float   #-}
 {-# SPECIALIZE instance Window UU DIM1 Double  #-}
 {-# SPECIALIZE instance Window UU DIM1 Word8   #-}
 {-# SPECIALIZE instance Window UU DIM1 Word16  #-}
 {-# SPECIALIZE instance Window UU DIM1 Word32  #-}
 {-# SPECIALIZE instance Window UU DIM1 Word64  #-}


-- Target ---------------------------------------------------------------------
instance U.Unbox e => Target UU e (UM.IOVector e) where
 data Buffer UU e 
  = UUBuffer !(UM.IOVector e)

 unsafeNewBuffer len
  = liftM UUBuffer (UM.unsafeNew len)
 {-# INLINE unsafeNewBuffer #-}

 unsafeWriteBuffer (UUBuffer mvec) ix
  = UM.unsafeWrite mvec ix
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer (UUBuffer mvec) bump
  = do  mvec'   <- UM.unsafeGrow mvec bump
        return  $ UUBuffer mvec'
 {-# INLINE unsafeGrowBuffer #-}

 unsafeFreezeBuffer sh (UUBuffer mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  UUArray sh vec
 {-# INLINE unsafeFreezeBuffer #-}

 unsafeSliceBuffer start len (UUBuffer mvec)
  = do  let mvec'  = UM.unsafeSlice start len mvec
        return $ UUBuffer mvec'
 {-# INLINE unsafeSliceBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE touchBuffer #-}

 {-# SPECIALIZE instance Target UU Int    (UM.IOVector Int)    #-}
 {-# SPECIALIZE instance Target UU Float  (UM.IOVector Float)  #-}
 {-# SPECIALIZE instance Target UU Double (UM.IOVector Double) #-}
 {-# SPECIALIZE instance Target UU Word8  (UM.IOVector Word8)  #-}
 {-# SPECIALIZE instance Target UU Word16 (UM.IOVector Word16) #-}
 {-# SPECIALIZE instance Target UU Word32 (UM.IOVector Word32) #-}
 {-# SPECIALIZE instance Target UU Word64 (UM.IOVector Word64) #-}


instance Unpack (Buffer UU e) (UM.IOVector e) where
 unpack (UUBuffer vec) = vec `seq` vec
 repack !_ !vec        = UUBuffer vec
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-- Conversions ----------------------------------------------------------------
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


-------------------------------------------------------------------------------
-- | O(1). Produce a nested array by taking slices from some array of elements.
--   
--   This is a constant time operation, as the representation for nested 
--   vectors simply wraps the starts lengths and elements vectors.
--
slicesUU :: Vector UU Int               -- ^ Segment starting positions.
         -> Vector UU Int               -- ^ Segment lengths.
         -> Vector r  a                 -- ^ Array elements.
         -> Vector UN (Vector r a)

slicesUU (UUArray _ starts) (UUArray _ lens) !elems
 = UNArray starts lens elems
{-# INLINE [1] slicesUU #-}
