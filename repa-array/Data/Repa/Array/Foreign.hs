
module Data.Repa.Array.Foreign
        ( F, Array (..), Buffer (..)
        , fromForeignPtr, toForeignPtr)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Index
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import qualified Foreign.ForeignPtr.Unsafe      as Unsafe
import Control.Monad.Primitive

-- | Arrays represented as foreign buffers in the C heap.
data F

-- | Foreign arrays.
instance (Shape sh, Storable a) => Bulk F sh a where
 data Array F sh a
        = FArray !sh !Int !(ForeignPtr a)

 extent (FArray sh _ _)
        = sh
 {-# INLINE extent #-}

 index (FArray sh offset fptr) ix
        | not $ inShapeRange zeroDim sh ix
        = error "repa-bulk.index[F]: out of range"

        | otherwise
        = unsafeInlineIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (offset + toIndex sh ix)
 {-# INLINE index #-}


-- Window ---------------------------------------------------------------------
instance Window F DIM1 a where
 window (Z :. start) sh' (FArray _ offset ptr)
        = FArray sh' (offset + start) ptr
 {-# INLINE window #-}


-- Target ---------------------------------------------------------------------
instance Storable a => Target F a where
 data Buffer F a
        = FBuffer !Int !(ForeignPtr a)

 unsafeNewBuffer len
  = do  let (proxy :: a) = undefined
        ptr     <- mallocBytes (sizeOf proxy * len)
        _       <- peek ptr  `asTypeOf` return proxy
        
        fptr    <- newForeignPtr finalizerFree ptr
        return  $ FBuffer len fptr
 {-# INLINE unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (FBuffer _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) ix x
 {-# INLINE unsafeWriteBuffer #-}

 unsafeFreezeBuffer !sh (FBuffer _len fptr)
  =     return  $ FArray sh 0 fptr
 {-# INLINE unsafeFreezeBuffer #-}

 unsafeSliceBuffer = error "UF slice not finished"

 touchBuffer (FBuffer _ fptr)
  = touchForeignPtr fptr
 {-# INLINE touchBuffer #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr
        :: Shape sh
        => sh -> ForeignPtr e -> Array F sh e
fromForeignPtr !sh !fptr
        = FArray sh 0 fptr
{-# INLINE fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array F sh e -> ForeignPtr e
toForeignPtr (FArray _ _ fptr)
        = fptr
{-# INLINE toForeignPtr #-}
