{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Unsafe.Unboxed
        ( U.U    (..)
        , U.Unbox
        , Array  (..)
        , Buffer (..)
        , Window (..)

        -- * Conversions
        , fromVector, toVector)
where
import Data.Repa.Array.Window
import Data.Repa.Array.Delayed
import Data.Repa.Array.Checked
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Control.Monad
import Data.Word
import qualified Data.Repa.Array.Material.Safe.Base     as S
import qualified Data.Repa.Array.Material.Unsafe.Base   as U
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Unsafe Unboxed arrays.
instance Repr U.U where
 type Safe   U.U = S.U
 type Unsafe U.U = U.U
 repr            = U.U
 {-# INLINE repr #-}


-- | Unboxed arrays.
instance Repr S.U where
 type Safe   S.U  = S.U
 type Unsafe S.U  = U.U
 repr             = S.U
 {-# INLINE repr #-}
 

---------------------------------------------------------------------
-- | Unsafe Unboxed arrays.
instance (Shape sh, U.Unbox a) => Bulk U.U sh a where
 data Array U.U sh a            = UUArray !sh !(U.Vector a)
 extent (UUArray sh _)          = sh
 index  (UUArray sh vec) ix     = vec `U.unsafeIndex` (toIndex sh ix)
 safe   arr                     = SUArray $ checked arr
 unsafe arr                     = arr
 {-# INLINE_ARRAY extent #-}
 {-# INLINE_ARRAY index  #-}
 {-# INLINE_ARRAY safe   #-}
 {-# INLINE_ARRAY unsafe #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 ()      #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Bool    #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Char    #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk U.U DIM1 Word64  #-}

deriving instance (Show sh, Show e, U.Unbox e) => Show (Array U.U sh e)


-- | Unboxed arrays.
instance (Shape sh, U.Unbox e) => Bulk S.U sh e where
 data Array S.U sh e            = SUArray !(Array (K U.U) sh e)
 extent (SUArray inner)         = extent inner
 index  (SUArray inner) ix      = index  inner ix
 safe   arr                     = arr
 unsafe (SUArray inner)         = unchecked inner
 {-# INLINE_ARRAY extent #-}
 {-# INLINE_ARRAY index  #-}
 {-# INLINE_ARRAY safe   #-}
 {-# INLINE_ARRAY unsafe #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 ()      #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Bool    #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Char    #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk S.U DIM1 Word64  #-}

deriving instance (Show sh, Show e, U.Unbox e) => Show (Array S.U sh e)


-- Window ---------------------------------------------------------------------
-- | Unsafe Unboxed windows.
instance U.Unbox a => Window U.U DIM1 a where
 window (Z :. start) (Z :. len) (UUArray _sh vec)
        = UUArray (Z :. len) (U.slice start len vec)
 {-# INLINE_ARRAY window #-}
 {-# SPECIALIZE instance Window U.U DIM1 Int     #-}
 {-# SPECIALIZE instance Window U.U DIM1 Float   #-}
 {-# SPECIALIZE instance Window U.U DIM1 Double  #-}
 {-# SPECIALIZE instance Window U.U DIM1 Word8   #-}
 {-# SPECIALIZE instance Window U.U DIM1 Word16  #-}
 {-# SPECIALIZE instance Window U.U DIM1 Word32  #-}
 {-# SPECIALIZE instance Window U.U DIM1 Word64  #-}


-- Target ---------------------------------------------------------------------
-- | Unsafe Unboxed buffers.
instance U.Unbox e => Target U.U e (UM.IOVector e) where
 data Buffer U.U e 
  = UUBuffer !(UM.IOVector e)

 unsafeNewBuffer len
  = liftM UUBuffer (UM.unsafeNew len)
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 unsafeWriteBuffer (UUBuffer mvec) ix
  = UM.unsafeWrite mvec ix
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

 unsafeGrowBuffer (UUBuffer mvec) bump
  = do  mvec'   <- UM.unsafeGrow mvec bump
        return  $ UUBuffer mvec'
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer sh (UUBuffer mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  UUArray sh vec
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeSliceBuffer start len (UUBuffer mvec)
  = do  let mvec'  = UM.unsafeSlice start len mvec
        return $ UUBuffer mvec'
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE_ARRAY touchBuffer #-}

 {-# SPECIALIZE instance Target U.U Int    (UM.IOVector Int)    #-}
 {-# SPECIALIZE instance Target U.U Float  (UM.IOVector Float)  #-}
 {-# SPECIALIZE instance Target U.U Double (UM.IOVector Double) #-}
 {-# SPECIALIZE instance Target U.U Word8  (UM.IOVector Word8)  #-}
 {-# SPECIALIZE instance Target U.U Word16 (UM.IOVector Word16) #-}
 {-# SPECIALIZE instance Target U.U Word32 (UM.IOVector Word32) #-}
 {-# SPECIALIZE instance Target U.U Word64 (UM.IOVector Word64) #-}


instance Unpack (Buffer U.U e) (UM.IOVector e) where
 unpack (UUBuffer vec) = vec `seq` vec
 repack !_ !vec        = UUBuffer vec
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromVector
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U.U sh e
fromVector sh vec
        = UUArray sh vec
{-# INLINE_ARRAY fromVector #-}


-- | O(1). Unpack an unboxed vector from an array.
toVector
        :: U.Unbox e
        => Array U.U sh e -> U.Vector e
toVector (UUArray _ vec)
        = vec
{-# INLINE_ARRAY toVector #-}

