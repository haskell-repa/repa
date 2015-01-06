
module Data.Repa.Array.Unsafe.Foreign
        ( UF, Array (..))
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Shape
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import System.IO.Unsafe
import qualified Foreign.ForeignPtr.Unsafe      as Unsafe


-- | Arrays represented as foreign buffers in the C heap.
data UF

-- | Unsafe Foreign arrays.
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
        = UFBuffer
        { -- | Starting position of data, in elements.
          _ufBufferStart :: !Int

          -- | Length of buffer, in elements.
        , _ufBufferLen   :: !Int 

          -- | Pointer to buffer data.
        , _ufBufferFPtr  :: !(ForeignPtr a) }

 unsafeNewBuffer len
  = do  let (proxy :: a) = undefined
        ptr     <- mallocBytes (sizeOf proxy * len)
        _       <- peek ptr  `asTypeOf` return proxy
        
        fptr    <- newForeignPtr finalizerFree ptr
        return  $ UFBuffer 0 len fptr
 {-# INLINE unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (UFBuffer start _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) (start + ix) x
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer (UFBuffer start len fptr) bump
  =  withForeignPtr fptr $ \ptr 
  -> do let (proxy :: a) = undefined
        let len'         = len + bump
        let bytesLen'    = sizeOf proxy * len'
        let bytesStart   = sizeOf proxy * start

        ptr'            <- mallocBytes bytesLen'
        copyBytes ptr' (plusPtr ptr bytesStart) bytesLen'

        fptr'   <- newForeignPtr finalizerFree ptr'
        return  $ UFBuffer 0 len' fptr'
 {-# INLINE unsafeGrowBuffer #-}

 unsafeFreezeBuffer !sh (UFBuffer start _len fptr)
  =     return  $ UFArray sh start fptr
 {-# INLINE unsafeFreezeBuffer #-}

 unsafeSliceBuffer start' len (UFBuffer start _len fptr)
  =     return  $ UFBuffer (start + start') len fptr
 {-# INLINE unsafeSliceBuffer #-}

 touchBuffer (UFBuffer _ _ fptr)
  = touchForeignPtr fptr
 {-# INLINE touchBuffer #-}


