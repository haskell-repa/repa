{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Safe.Unboxed
        ( S.U    (..)
        , U.Unbox
        , Array  (..)
        , Buffer (..)
        , fromVector, toVector)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Shape
import Data.Repa.Array.Checked
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Control.Monad
import Data.Word
import Data.Repa.Array.Material.Unsafe.Unboxed   (Buffer(..), Array (..))
import qualified Data.Repa.Array.Material.Safe.Base             as S
import qualified Data.Repa.Array.Material.Unsafe.Unboxed        as U
import qualified Data.Vector.Unboxed                            as U
import qualified Data.Vector.Unboxed.Mutable                    as UM
#include "repa-stream.h"


-- Window ---------------------------------------------------------------------
-- | Unboxed windows.
instance U.Unbox a => Window S.U DIM1 a where
 window (Z :. start) (Z :. len) (SUArray (KArray (UUArray _sh vec)))
        = SUArray (KArray (UUArray (Z :. len) (U.slice start len vec)))
 {-# INLINE_ARRAY window #-}
 {-# SPECIALIZE instance Window S.U DIM1 ()      #-}
 {-# SPECIALIZE instance Window S.U DIM1 Bool    #-}
 {-# SPECIALIZE instance Window S.U DIM1 Char    #-}
 {-# SPECIALIZE instance Window S.U DIM1 Int     #-}
 {-# SPECIALIZE instance Window S.U DIM1 Float   #-}
 {-# SPECIALIZE instance Window S.U DIM1 Double  #-}
 {-# SPECIALIZE instance Window S.U DIM1 Word8   #-}
 {-# SPECIALIZE instance Window S.U DIM1 Word16  #-}
 {-# SPECIALIZE instance Window S.U DIM1 Word32  #-}
 {-# SPECIALIZE instance Window S.U DIM1 Word64  #-}


-------------------------------------------------------------------------------
-- | Unboxed buffers.
instance U.Unbox e
      => Target S.U e (UM.IOVector e) where
 data Buffer S.U e        
  = SUBuffer !(UM.IOVector e)

 unsafeNewBuffer len
  = liftM SUBuffer (UM.unsafeNew len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeWriteBuffer (SUBuffer mvec) ix
  = UM.unsafeWrite mvec ix
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer (SUBuffer mvec) bump
  = do  mvec'   <- UM.unsafeGrow mvec bump
        return  $ SUBuffer mvec'
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer sh (SUBuffer mvec)
  = do  uuarr   <- unsafeFreezeBuffer sh (KBuffer (UUBuffer mvec))
        return  $ SUArray uuarr

 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeSliceBuffer start len (SUBuffer mvec)
  = do  let mvec'  = UM.unsafeSlice start len mvec
        return $ SUBuffer mvec'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}

 {-# SPECIALIZE instance Target S.U ()     (UM.IOVector ())     #-}
 {-# SPECIALIZE instance Target S.U Char   (UM.IOVector Char)   #-}
 {-# SPECIALIZE instance Target S.U Int    (UM.IOVector Int)    #-}
 {-# SPECIALIZE instance Target S.U Float  (UM.IOVector Float)  #-}
 {-# SPECIALIZE instance Target S.U Double (UM.IOVector Double) #-}
 {-# SPECIALIZE instance Target S.U Word8  (UM.IOVector Word8)  #-}
 {-# SPECIALIZE instance Target S.U Word16 (UM.IOVector Word16) #-}
 {-# SPECIALIZE instance Target S.U Word32 (UM.IOVector Word32) #-}
 {-# SPECIALIZE instance Target S.U Word64 (UM.IOVector Word64) #-}


-- | Unpack unboxed buffers.
instance Unpack (Buffer S.U e) (UM.IOVector e) where
 unpack (SUBuffer vec)  = vec `seq` vec
 repack !_ !vec         = SUBuffer vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromVector  :: (Shape sh, U.Unbox e)
            => sh -> U.Vector e -> Array S.U sh e
fromVector sh vec
        = SUArray $ checked $ U.fromVector sh vec
{-# INLINE_ARRAY fromVector #-}


-- | O(1). Unpack an unboxed vector from an array.
toVector :: U.Unbox e
         => Array S.U sh e -> U.Vector e
toVector (SUArray arr)
        = U.toVector (unchecked arr)
{-# INLINE_ARRAY toVector #-}

