{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Safe.Unboxed
        ( U(..),        U.Unbox
        , Array(..)
        , Buffer(..)
        , fromVector, toVector)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Shape
import Data.Repa.Array.Checked
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Material.Unsafe.Unboxed  (U(..), Buffer(..), Array (..))
import qualified Data.Repa.Array.Material.Unsafe.Unboxed        as UU
import qualified Data.Vector.Unboxed                            as U
import qualified Data.Vector.Unboxed.Mutable                    as UM
import Control.Monad
import Data.Word


-- Window ---------------------------------------------------------------------
-- | Unboxed windows.
instance U.Unbox a => Window U DIM1 a where
 window (Z :. start) (Z :. len) (UArray (KArray (UUArray _sh vec)))
        = UArray (KArray (UUArray (Z :. len) (U.slice start len vec)))
 {-# INLINE window #-}
 {-# SPECIALIZE instance Window U DIM1 ()      #-}
 {-# SPECIALIZE instance Window U DIM1 Bool    #-}
 {-# SPECIALIZE instance Window U DIM1 Char    #-}
 {-# SPECIALIZE instance Window U DIM1 Int     #-}
 {-# SPECIALIZE instance Window U DIM1 Float   #-}
 {-# SPECIALIZE instance Window U DIM1 Double  #-}
 {-# SPECIALIZE instance Window U DIM1 Word8   #-}
 {-# SPECIALIZE instance Window U DIM1 Word16  #-}
 {-# SPECIALIZE instance Window U DIM1 Word32  #-}
 {-# SPECIALIZE instance Window U DIM1 Word64  #-}


-------------------------------------------------------------------------------
-- | Unboxed buffers.
instance U.Unbox e
      => Target U e (UM.IOVector e) where
 data Buffer U e        
  = UBuffer !(UM.IOVector e)

 unsafeNewBuffer len
  = liftM UBuffer (UM.unsafeNew len)
 {-# INLINE unsafeNewBuffer #-}

 unsafeWriteBuffer (UBuffer mvec) ix
  = UM.unsafeWrite mvec ix
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer (UBuffer mvec) bump
  = do  mvec'   <- UM.unsafeGrow mvec bump
        return  $ UBuffer mvec'
 {-# INLINE unsafeGrowBuffer #-}

 unsafeFreezeBuffer sh (UBuffer mvec)
  = do  uuarr   <- unsafeFreezeBuffer sh (KBuffer (UUBuffer mvec))
        return  $ UArray uuarr

 {-# INLINE unsafeFreezeBuffer #-}

 unsafeSliceBuffer start len (UBuffer mvec)
  = do  let mvec'  = UM.unsafeSlice start len mvec
        return $ UBuffer mvec'
 {-# INLINE unsafeSliceBuffer #-}

 touchBuffer _ 
  = return ()
 {-# INLINE touchBuffer #-}

 {-# SPECIALIZE instance Target U ()     (UM.IOVector ())     #-}
 {-# SPECIALIZE instance Target U Char   (UM.IOVector Char)   #-}
 {-# SPECIALIZE instance Target U Int    (UM.IOVector Int)    #-}
 {-# SPECIALIZE instance Target U Float  (UM.IOVector Float)  #-}
 {-# SPECIALIZE instance Target U Double (UM.IOVector Double) #-}
 {-# SPECIALIZE instance Target U Word8  (UM.IOVector Word8)  #-}
 {-# SPECIALIZE instance Target U Word16 (UM.IOVector Word16) #-}
 {-# SPECIALIZE instance Target U Word32 (UM.IOVector Word32) #-}
 {-# SPECIALIZE instance Target U Word64 (UM.IOVector Word64) #-}


instance Unpack (Buffer U e) (UM.IOVector e) where
 unpack (UBuffer vec)   = vec `seq` vec
 repack !_ !vec         = UBuffer vec
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromVector  :: (Shape sh, U.Unbox e)
            => sh -> U.Vector e -> Array U sh e
fromVector sh vec
        = UArray $ checked $ UU.fromVector sh vec
{-# INLINE [1] fromVector #-}


-- | O(1). Unpack an unboxed vector from an array.
toVector :: U.Unbox e
         => Array U sh e -> U.Vector e
toVector (UArray arr)
        = UU.toVector (unchecked arr)
{-# INLINE [1] toVector #-}

