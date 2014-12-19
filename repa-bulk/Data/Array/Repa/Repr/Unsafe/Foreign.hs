
module Data.Array.Repa.Repr.Unsafe.Foreign
        (UF, Array (..))
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
data UF

instance (Shape sh, Storable a) => Bulk UF sh a where
 data Array UF sh a
        = UFArray !sh !Int !(ForeignPtr a)

 extent (UFArray sh _ _)
        = sh
 {-# INLINE extent #-}

 index (UFArray sh offset fptr) ix
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (offset + toIndex sh ix)
 {-# INLINE index #-}
 
 
-- Target ---------------------------------------------------------------------
instance Storable a => Target UF a where
 data Buffer UF a
  = UFBuffer !Int !(ForeignPtr a)

 unsafeNewBuffer len
  = do  let (proxy :: a) = undefined
        ptr     <- mallocBytes (sizeOf proxy * len)
        _       <- peek ptr  `asTypeOf` return proxy
        
        fptr    <- newForeignPtr finalizerFree ptr
        return  $ UFBuffer len fptr
 {-# INLINE unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (UFBuffer _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) ix x
 {-# INLINE unsafeWriteBuffer #-}

 unsafeFreezeBuffer !sh (UFBuffer _len fptr)
  =     return  $ UFArray sh 0 fptr
 {-# INLINE unsafeFreezeBuffer #-}

 unsafeSliceBuffer = error "UF slice not finished"

 touchBuffer (UFBuffer _ fptr)
  = touchForeignPtr fptr
 {-# INLINE touchBuffer #-}
