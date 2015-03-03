
module Data.Repa.Array.Material.Unboxed
        ( U      (..)
        , Name   (..)
        , Array  (..)
        , Buffer (..)
        , U.Unbox

        -- * Conversions
        , fromUnboxed,  toUnboxed)
where
import Data.Repa.Array.Window
import Data.Repa.Array.Delayed
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Control.Monad
import Data.Word
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
#include "repa-array.h"


-- | Layout an array as a flat vector of unboxed elements.
--
--   This is the most efficient representation for numerical data.
--
--   The implementation uses @Data.Vector.Unboxed@ which picks an efficient,
--   specialised representation for every element type. In particular,
--   unboxed vectors of pairs are represented as pairs of unboxed vectors.
--
--   UNSAFE: Indexing into raw material arrays is not bounds checked.
--   You may want to wrap this with a Checked layout as well.
--
data U = Unboxed { unboxedLength :: !Int }
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | Unboxed arrays.
instance Layout U where
 data Name  U                   = U
 type Index U                   = Int
 name                           = U
 create U len                   = Unboxed len
 extent (Unboxed len)           = len
 toIndex   _ ix                 = ix
 fromIndex _ ix                 = ix
 {-# INLINE_ARRAY name      #-}
 {-# INLINE_ARRAY create    #-}
 {-# INLINE_ARRAY extent    #-}
 {-# INLINE_ARRAY toIndex   #-}
 {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name U)
deriving instance Show (Name U)


-------------------------------------------------------------------------------
-- | Unboxed arrays.
instance U.Unbox a => Bulk U a where
 data Array U a                 = UArray !(U.Vector a)
 layout (UArray vec)            = Unboxed (U.length vec)
 index  (UArray vec) ix         = U.unsafeIndex vec ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}
 {-# SPECIALIZE instance Bulk U ()      #-}
 {-# SPECIALIZE instance Bulk U Bool    #-}
 {-# SPECIALIZE instance Bulk U Char    #-}
 {-# SPECIALIZE instance Bulk U Int     #-}
 {-# SPECIALIZE instance Bulk U Float   #-}
 {-# SPECIALIZE instance Bulk U Double  #-}
 {-# SPECIALIZE instance Bulk U Word8   #-}
 {-# SPECIALIZE instance Bulk U Word16  #-}
 {-# SPECIALIZE instance Bulk U Word32  #-}
 {-# SPECIALIZE instance Bulk U Word64  #-}

deriving instance (Show a, U.Unbox a) => Show (Array U a)


instance Unpack (Array U a) (U.Vector a) where
 unpack (UArray vec)    = vec
 repack !_ !vec         = UArray vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | Windowing Unboxed arrays.
instance U.Unbox a => Windowable U a where
 window st len (UArray vec)
        = UArray (U.slice st len vec)
 {-# INLINE_ARRAY window #-}
 {-# SPECIALIZE instance Windowable U Int     #-}
 {-# SPECIALIZE instance Windowable U Float   #-}
 {-# SPECIALIZE instance Windowable U Double  #-}
 {-# SPECIALIZE instance Windowable U Word8   #-}
 {-# SPECIALIZE instance Windowable U Word16  #-}
 {-# SPECIALIZE instance Windowable U Word32  #-}
 {-# SPECIALIZE instance Windowable U Word64  #-}


-------------------------------------------------------------------------------
-- | Unboxed buffers.
instance U.Unbox a => Target U a where
 data Buffer s U a
  = UBuffer !(UM.MVector s a)

 unsafeNewBuffer (Unboxed len)
  = liftM UBuffer (UM.unsafeNew len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer (UBuffer mvec) ix
  = UM.unsafeRead mvec ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer (UBuffer mvec) ix
  = UM.unsafeWrite mvec ix
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer (UBuffer mvec) bump
  = do  mvec'   <- UM.unsafeGrow mvec bump
        return  $ UBuffer mvec'
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (UBuffer mvec)
  = do  vec     <- U.unsafeFreeze mvec
        return  $  UArray vec
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer (UArray mvec)
  = liftM UBuffer (U.unsafeThaw mvec)
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer st len (UBuffer mvec)
  = do  let mvec'  = UM.unsafeSlice st len mvec
        return $ UBuffer mvec'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer _
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (UBuffer mvec)
   = Unboxed (UM.length mvec)

 {-# SPECIALIZE instance Target U Int    #-}
 {-# SPECIALIZE instance Target U Float  #-}
 {-# SPECIALIZE instance Target U Double #-}
 {-# SPECIALIZE instance Target U Word8  #-}
 {-# SPECIALIZE instance Target U Word16 #-}
 {-# SPECIALIZE instance Target U Word32 #-}
 {-# SPECIALIZE instance Target U Word64 #-}


instance Unpack (Buffer s U a) (UM.MVector s a) where
 unpack (UBuffer vec)  = vec `seq` vec
 repack !_ !vec        = UBuffer vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromUnboxed :: U.Unbox a
            => U.Vector a -> Array U a
fromUnboxed vec = UArray vec
{-# INLINE_ARRAY fromUnboxed #-}


-- | O(1). Unwrap an unboxed vector from an array.
toUnboxed   :: U.Unbox a
            => Array U a -> U.Vector a
toUnboxed (UArray vec) = vec
{-# INLINE_ARRAY toUnboxed #-}

