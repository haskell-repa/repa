
module Data.Array.Repa.Repr.Foreign
        ( F, Array (..)
        , fromForeignPtr, toForeignPtr)
where
import Data.Array.Repa.Bulk.Target
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Shape
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import qualified Foreign.ForeignPtr.Unsafe      as Unsafe


-- | Arrays represented as foreign buffers in the C heap.
data F

instance (Shape sh, Storable a) => Bulk F sh a where
 data Array F sh a
        = FArray !sh !Int !(ForeignPtr a)

 linearIndex (FArray _ len fptr) ix
        | ix < len
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr ix

        | otherwise
        = error "repa-flow.linearIndex[F]: index out of bounds"
 {-# INLINE linearIndex #-}
 
 extent (FArray sh _ _)
        = sh
 {-# INLINE extent #-}

 slice = error "UF slice not finished"

 
-- Target ---------------------------------------------------------------------
instance Storable a => Target F a where
 data Buffer F a
  = FBuffer !Int !(ForeignPtr a)

 unsafeNewBuffer len
  = do  let (proxy :: a) = undefined
        ptr              <- mallocBytes (sizeOf proxy * len)
        _                <- peek ptr  `asTypeOf` return proxy
        
        fptr             <- newForeignPtr finalizerFree ptr
        return           $ FBuffer len fptr
 {-# INLINE unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (FBuffer _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) ix x
 {-# INLINE unsafeWriteBuffer #-}

 unsafeFreezeBuffer !sh (FBuffer len fptr)
  =     return  $ FArray sh len fptr
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
        = FArray sh (size sh) fptr
{-# INLINE fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array F sh e -> ForeignPtr e
toForeignPtr (FArray _ _ fptr)
        = fptr
{-# INLINE toForeignPtr #-}
