{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Safe.Boxed
        ( B      (..)
        , Array  (..)
        , Buffer (..)
        , fromVector, toVector)
where
import Data.Repa.Eval.Array
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Checked
import Data.Repa.Array.Shape
import Data.Repa.Array.Material.Unsafe.Boxed
import Data.Repa.Fusion.Unpack
import qualified Data.Repa.Array.Material.Safe.Base     as S
import qualified Data.Vector                            as V
import qualified Data.Vector.Mutable                    as VM
import Control.Monad
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Boxed windows.
instance Window S.B DIM1 a where
 window (Z :. start) (Z :. len) (SBArray (KArray (UBArray _sh vec)))
        = SBArray (KArray (UBArray (Z :. len) (V.slice start len vec)))
 {-# INLINE_ARRAY window #-}


-------------------------------------------------------------------------------
-- | Boxed buffers.
instance Target S.B a (VM.IOVector a) where
 data Buffer S.B a 
  = SBBuffer (VM.IOVector a)

 unsafeNewBuffer len
  = liftM SBBuffer (VM.unsafeNew len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeWriteBuffer (SBBuffer mvec) ix
  = VM.unsafeWrite mvec ix
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer (SBBuffer mvec) bump
  = do  mvec'   <- VM.unsafeGrow mvec bump
        return  $ SBBuffer mvec'
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeSliceBuffer start len (SBBuffer mvec)
  = do  let mvec'  = VM.unsafeSlice start len mvec
        return  $  SBBuffer mvec'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 unsafeFreezeBuffer sh (SBBuffer mvec)     
  = do  vec     <- V.unsafeFreeze mvec
        return  $  SBArray (KArray (UBArray sh vec))
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}


-- | Unpack boxed buffers.
instance Unpack (Buffer S.B a) (VM.IOVector a) where
 unpack (SBBuffer vec) = vec
 repack _ vec          = SBBuffer vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | O(1). Wrap a boxed vector as an array.
fromVector :: Shape sh => sh -> V.Vector e -> Array S.B sh e
fromVector sh vec = SBArray $ checked $ UBArray sh vec
{-# INLINE_ARRAY fromVector #-}


-- | O(1). Unpack a boxed vector from an array.
toVector :: Array S.B sh e -> V.Vector e
toVector (SBArray (KArray (UBArray _ vec))) = vec
{-# INLINE_ARRAY toVector #-}

