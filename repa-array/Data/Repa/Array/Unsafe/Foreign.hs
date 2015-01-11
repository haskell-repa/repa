
module Data.Repa.Array.Unsafe.Foreign
        ( UF, Array (..))
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Index
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import System.IO.Unsafe
import qualified Foreign.ForeignPtr.Unsafe      as Unsafe
import Data.Repa.Fusion.Unpack


-------------------------------------------------------------------------------
-- | Arrays represented as foreign buffers in the C heap.
data UF

instance (Shape sh, Storable a) => Bulk UF sh a where
 data Array UF sh a       = UFArray !sh !Int !(ForeignPtr a)
 extent (UFArray sh _ _)  = sh
 index (UFArray sh offset fptr) ix
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (offset + toIndex sh ix)
 {-# INLINE extent #-}
 {-# INLINE index #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word64  #-}


instance Unpack (Array UF DIM1 a) (Int, Int, ForeignPtr a) where
 unpack (UFArray (Z :. len) offset fptr) = (len, offset, fptr)
 repack _ (len, offset, fptr)            = UFArray (Z :. len) offset fptr


deriving instance (Show sh, Show e) => Show (Array UF sh e)


-- Window ---------------------------------------------------------------------
instance Storable a 
      => Window UF DIM1 a where
 window (Z :. start) sh' (UFArray _ offset ptr)
        = UFArray sh' (offset + start) ptr
 {-# INLINE window #-}
 {-# SPECIALIZE instance Window UF DIM1 Int     #-}
 {-# SPECIALIZE instance Window UF DIM1 Float   #-}
 {-# SPECIALIZE instance Window UF DIM1 Double  #-}
 {-# SPECIALIZE instance Window UF DIM1 Word8   #-}
 {-# SPECIALIZE instance Window UF DIM1 Word16  #-}
 {-# SPECIALIZE instance Window UF DIM1 Word32  #-}
 {-# SPECIALIZE instance Window UF DIM1 Word64  #-}


-- Target ---------------------------------------------------------------------
instance Storable a 
      => Target UF a (Int, Int, ForeignPtr a) where
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

 {-# SPECIALIZE instance Target UF Int    (Int, Int, ForeignPtr Int)    #-}
 {-# SPECIALIZE instance Target UF Float  (Int, Int, ForeignPtr Float)  #-}
 {-# SPECIALIZE instance Target UF Double (Int, Int, ForeignPtr Double) #-}
 {-# SPECIALIZE instance Target UF Word8  (Int, Int, ForeignPtr Word8)  #-}
 {-# SPECIALIZE instance Target UF Word16 (Int, Int, ForeignPtr Word16) #-}
 {-# SPECIALIZE instance Target UF Word32 (Int, Int, ForeignPtr Word32) #-}
 {-# SPECIALIZE instance Target UF Word64 (Int, Int, ForeignPtr Word64) #-}


instance Unpack (Buffer UF a) (Int, Int, ForeignPtr a) where
 unpack (UFBuffer start len fptr) = (start, len, fptr)
 repack _ (start, len, fptr)      = UFBuffer start len fptr
 {-# INLINE unpack #-}
 {-# INLINE repack #-}
