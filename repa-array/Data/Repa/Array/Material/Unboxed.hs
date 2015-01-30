
module Data.Repa.Array.Material.Unboxed
        ( U      (..)
        , Name   (..)
        , Array  (..)
        , Buffer (..)
        , U.Unbox

        -- * Conversions
        , fromVector, toVector)
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
data U  = Unboxed { unboxedLength :: Int }

deriving instance Eq U
deriving instance Show U


-------------------------------------------------------------------------------
-- | Unboxed arrays.
instance Layout U where
 data Name  U                   = U
 type Index U                   = Int
 create U len                   = Unboxed len
 extent (Unboxed len)           = len
 toIndex   _ ix                 = ix
 fromIndex _ ix                 = ix
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
 data Buffer U a 
  = UBuffer !(UM.IOVector a)

 unsafeNewBuffer (Unboxed len)
  = liftM UBuffer (UM.unsafeNew len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

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

 unsafeSliceBuffer st len (UBuffer mvec)
  = do  let mvec'  = UM.unsafeSlice st len mvec
        return $ UBuffer mvec'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}

 {-# SPECIALIZE instance Target U Int    #-}
 {-# SPECIALIZE instance Target U Float  #-}
 {-# SPECIALIZE instance Target U Double #-}
 {-# SPECIALIZE instance Target U Word8  #-}
 {-# SPECIALIZE instance Target U Word16 #-}
 {-# SPECIALIZE instance Target U Word32 #-}
 {-# SPECIALIZE instance Target U Word64 #-}


instance Unpack (Buffer U e) (UM.IOVector e) where
 unpack (UBuffer vec)  = vec `seq` vec
 repack !_ !vec        = UBuffer vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromVector
        :: U.Unbox e
        => U.Vector e -> Array U e
fromVector vec = UArray vec
{-# INLINE_ARRAY fromVector #-}


-- | O(1). Unpack an unboxed vector from an array.
toVector 
        :: U.Unbox e
        => Array U e -> U.Vector e
toVector (UArray vec) = vec
{-# INLINE_ARRAY toVector #-}

