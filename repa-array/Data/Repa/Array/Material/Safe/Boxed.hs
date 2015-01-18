{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Safe.Boxed
        ( B(..)
        , Array (..)
        , fromVector, toVector)
where
import Data.Repa.Eval.Array
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Checked
import Data.Repa.Array.Shape
import Data.Repa.Array.Material.Unsafe.Boxed
import Data.Repa.Fusion.Unpack
import qualified Data.Vector                    as V
import qualified Data.Vector.Mutable            as VM
import Control.Monad


-- Window ---------------------------------------------------------------------
-- | Boxed windows.
instance Window B DIM1 a where
 window (Z :. start) (Z :. len) (BArray (KArray (UBArray _sh vec)))
        = BArray (KArray (UBArray (Z :. len) (V.slice start len vec)))
 {-# INLINE window #-}


-- Target ---------------------------------------------------------------------
-- | Boxed buffers.
instance Target B a (VM.IOVector a) where
 data Buffer B a 
  = BBuffer (VM.IOVector a)

 unsafeNewBuffer len
  = liftM BBuffer (VM.unsafeNew len)
 {-# INLINE unsafeNewBuffer #-}

 unsafeWriteBuffer (BBuffer mvec) ix
  = VM.unsafeWrite mvec ix
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer (BBuffer mvec) bump
  = do  mvec'   <- VM.unsafeGrow mvec bump
        return  $ BBuffer mvec'
 {-# INLINE unsafeGrowBuffer #-}

 unsafeSliceBuffer start len (BBuffer mvec)
  = do  let mvec'  = VM.unsafeSlice start len mvec
        return  $  BBuffer mvec'
 {-# INLINE unsafeSliceBuffer #-}

 unsafeFreezeBuffer sh (BBuffer mvec)     
  = do  vec     <- V.unsafeFreeze mvec
        return  $  BArray (KArray (UBArray sh vec))
 {-# INLINE unsafeFreezeBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE touchBuffer #-}


-- | Unpack boxed buffers.
instance Unpack (Buffer B a) (VM.IOVector a) where
 unpack (BBuffer vec) = vec
 repack _ vec         = BBuffer vec
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a boxed vector as an array.
fromVector :: Shape sh => sh -> V.Vector e -> Array B sh e
fromVector sh vec = BArray $ checked $ UBArray sh vec
{-# INLINE [1] fromVector #-}


-- | O(1). Unpack a boxed vector from an array.
toVector :: Array B sh e -> V.Vector e
toVector (BArray (KArray (UBArray _ vec))) = vec
{-# INLINE [1] toVector #-}

