{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Boxed () where

{-
        ( B    (..)
        , Array  (..)
        , Buffer (..)
        , Window (..))
where
import Data.Repa.Array.Window
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Data.Word
import Control.Monad
import qualified Data.Vector                            as V
import qualified Data.Vector.Mutable                    as VM
#include "repa-array.h"


-- | Representation tag for Unsafe arrays of Boxed elements.
data B = B


-- | Boxed arrays.
instance Bulk B a where
 data Array S.B sh a             = SBArray (Array (K U.B) sh a)
 index  (SBArray inner) ix       = index inner ix
 extent (SBArray inner)          = extent inner
 safe  arr                       = arr
 unsafe (SBArray (KArray inner)) = inner
 {-# INLINE_ARRAY index  #-}
 {-# INLINE_ARRAY extent #-}
 {-# INLINE_ARRAY safe   #-}
 {-# INLINE_ARRAY unsafe #-}

deriving instance (Show sh, Show a) => Show (Array S.B sh a)


-------------------------------------------------------------------------------
-- | Boxed windows.
instance Window U.B DIM1 a where
 window (Z :. start) (Z :. len) (UBArray _sh vec)
        = UBArray (Z :. len) (V.slice start len vec)
 {-# INLINE_ARRAY window #-}


-- Target ---------------------------------------------------------------------
-- | Boxed buffers.
instance Target U.B e (VM.IOVector e) where
 data Buffer U.B e 
  = UBBuffer !(VM.IOVector e)

 unsafeNewBuffer len
  = liftM UBBuffer (VM.unsafeNew len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeWriteBuffer (UBBuffer mvec) ix
  = VM.unsafeWrite mvec ix
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer (UBBuffer mvec) bump
  = do  mvec'   <- VM.unsafeGrow mvec bump
        return  $ UBBuffer mvec'
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer sh (UBBuffer mvec)     
  = do  vec     <- V.unsafeFreeze mvec
        return  $  UBArray sh vec
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeSliceBuffer start len (UBBuffer mvec)
  = do  let mvec'  = VM.unsafeSlice start len mvec
        return $ UBBuffer mvec'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}

 {-# SPECIALIZE instance Target U.B Int    (VM.IOVector Int)    #-}
 {-# SPECIALIZE instance Target U.B Float  (VM.IOVector Float)  #-}
 {-# SPECIALIZE instance Target U.B Double (VM.IOVector Double) #-}
 {-# SPECIALIZE instance Target U.B Word8  (VM.IOVector Word8)  #-}
 {-# SPECIALIZE instance Target U.B Word16 (VM.IOVector Word16) #-}
 {-# SPECIALIZE instance Target U.B Word32 (VM.IOVector Word32) #-}
 {-# SPECIALIZE instance Target U.B Word64 (VM.IOVector Word64) #-}


instance Unpack (Buffer U.B e) (VM.IOVector e) where
 unpack (UBBuffer vec) = vec `seq` vec
 repack !_ !vec        = UBBuffer vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}
-}
