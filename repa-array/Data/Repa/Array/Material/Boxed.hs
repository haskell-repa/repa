
module Data.Repa.Array.Material.Boxed
        ( B      (..)
        , Name   (..)
        , Array  (..)
        , Buffer (..)

        -- * Conversions
        , fromBoxed,    toBoxed)
where
import Data.Repa.Array.Window
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Data.Word
import Control.Monad
import qualified Data.Vector                            as V
import qualified Data.Vector.Mutable                    as VM
#include "repa-array.h"


-- | Layout an array as flat vector of boxed elements.
--
--   UNSAFE: Indexing into raw material arrays is not bounds checked.
--   You may want to wrap this with a Checked layout as well.
--
newtype B = Boxed { boxedLength :: Int }
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Boxed arrays.
instance Layout B where
 data Name  B                   = B
 type Index B                   = Int
 name                           = B
 create B len                   = Boxed len
 extent (Boxed len)             = len
 toIndex   _ ix                 = ix
 fromIndex _ ix                 = ix
 {-# INLINE_ARRAY name      #-}
 {-# INLINE_ARRAY create    #-}
 {-# INLINE_ARRAY extent    #-}
 {-# INLINE_ARRAY toIndex   #-}
 {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name B)
deriving instance Show (Name B)


------------------------------------------------------------------------------
-- | Boxed arrays.
instance Bulk B a where
 newtype Array B a               = BArray (V.Vector a)
 layout (BArray vec)             = Boxed (V.length vec)
 index  (BArray vec) ix          = V.unsafeIndex vec ix
 {-# INLINE_ARRAY layout  #-}
 {-# INLINE_ARRAY index   #-}

deriving instance Show a => Show (Array B a)


-------------------------------------------------------------------------------
-- | Boxed windows.
instance Windowable B a where
 window st len (BArray vec)
        = BArray (V.slice st len vec)
 {-# INLINE_ARRAY window #-}


-------------------------------------------------------------------------------
-- | Boxed buffers.
instance Target B a where
 newtype Buffer s B a
  = BBuffer (VM.MVector s a)

 unsafeNewBuffer (Boxed len)
  = liftM BBuffer (VM.unsafeNew len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeReadBuffer (BBuffer mvec) ix
  = VM.unsafeRead mvec ix
 {-# INLINE_ARRAY unsafeReadBuffer #-}

 unsafeWriteBuffer (BBuffer mvec) ix
  = VM.unsafeWrite mvec ix
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer (BBuffer mvec) bump
  = liftM BBuffer (VM.unsafeGrow mvec bump)
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (BBuffer mvec)
  = liftM BArray (V.unsafeFreeze mvec)
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeThawBuffer (BArray vec)
  = liftM BBuffer (V.unsafeThaw vec)
 {-# INLINE_ARRAY unsafeThawBuffer #-}

 unsafeSliceBuffer start len (BBuffer mvec)
  = let mvec'  = VM.unsafeSlice start len mvec
    in  return $ BBuffer mvec'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer _
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}

 bufferLayout (BBuffer mvec)
  = Boxed (VM.length mvec)
 {-# INLINE_ARRAY bufferLayout #-}

 {-# SPECIALIZE instance Target B Int    #-}
 {-# SPECIALIZE instance Target B Float  #-}
 {-# SPECIALIZE instance Target B Double #-}
 {-# SPECIALIZE instance Target B Word8  #-}
 {-# SPECIALIZE instance Target B Word16 #-}
 {-# SPECIALIZE instance Target B Word32 #-}
 {-# SPECIALIZE instance Target B Word64 #-}


instance Unpack (Buffer s B a) (VM.MVector s a) where
 unpack (BBuffer vec) = vec
 repack _ vec         = BBuffer vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | O(1). Wrap a boxed vector as an array.
fromBoxed :: V.Vector a -> Array B a
fromBoxed vec = BArray vec
{-# INLINE_ARRAY fromBoxed #-}


-- | O(1). Unwrap a boxed vector from an array.
toBoxed   :: Array B a -> V.Vector a
toBoxed (BArray vec) = vec
{-# INLINE_ARRAY toBoxed #-}


