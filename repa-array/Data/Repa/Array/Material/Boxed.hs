
module Data.Repa.Array.Material.Boxed
        ( B      (..)
        , Name   (..)
        , Array  (..)
        , Buffer (..)

        -- * Conversions
        , fromBoxed,    toBoxed

        -- * Utils
        , decimate)
where
import Data.Repa.Array.Meta.Window                      as A
import Data.Repa.Array.Generic.Index                    as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Target                 as A
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
data B = Boxed { boxedLength :: !Int }
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
 data Array B a                  = BArray !(V.Vector a)
 layout (BArray vec)             = Boxed (V.length vec)
 index  (BArray vec) ix          = V.unsafeIndex vec ix
 {-# INLINE_ARRAY layout  #-}
 {-# INLINE_ARRAY index   #-}

deriving instance Show a => Show (Array B a)


instance Eq a => Eq (Array B a) where
 (==) (BArray arr1) (BArray arr2) = arr1 == arr2
 {-# INLINE_ARRAY (==) #-}


-------------------------------------------------------------------------------
-- | Boxed windows.
instance Windowable B a where
 window st len (BArray vec)
        = BArray (V.slice st len vec)
 {-# INLINE_ARRAY window #-}


-------------------------------------------------------------------------------
-- | Boxed buffers.
instance Target B a where
 data Buffer B a
  = BBuffer !(VM.IOVector a)

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


instance Unpack (Buffer B a) (VM.IOVector a) where
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



-- | Scan through an array from front to back.
--   For pairs of successive elements, drop the second one when the given
--   predicate returns true.
--
--   This function can be used to remove duplicates from a sorted array.
--
--   TODO: generalise to other array types.
decimate
        :: (a -> a -> Bool)
        -> Array B a -> Array B a

decimate f arr
        | A.length arr == 0        
        = A.fromList B []

        | otherwise
        = fromBoxed
        $ V.cons (arr `A.index` 0)
                 (V.map  snd
                        $ V.filter (\(prev,  here) -> not $ f prev here)
                        $ V.zip (toBoxed arr) (V.tail $ toBoxed arr))

