
module Data.Repa.Array.Foreign
        ( F(..), Array (..), Buffer (..)
        , fromForeignPtr, toForeignPtr
        , eqVectorF)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Index
import Data.Repa.Fusion.Unpack
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import qualified Foreign.ForeignPtr.Unsafe      as Unsafe
import Control.Monad.Primitive
import Data.Word
import System.IO.Unsafe
import qualified Data.ByteString.Internal       as BS


-------------------------------------------------------------------------------
-- | Arrays represented as foreign buffers in the C heap.
data F = F

instance (Shape sh, Storable a) => Bulk F sh a where
 data Array F sh a      = FArray !sh !Int !(ForeignPtr a)
 extent (FArray sh _ _) = sh

 index (FArray sh offset fptr) ix
--        | not $ inShapeRange zeroDim sh ix               -- TODO: indexing bounds checks
--        = error "repa-bulk.index[F]: out of range"

        | otherwise
        = unsafeInlineIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (offset + toIndex sh ix)
 {-# INLINE extent #-}
 {-# INLINE index #-}
 {-# SPECIALIZE instance Bulk F DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk F DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk F DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word64  #-}

deriving instance (Show sh, Show a, Storable a) => Show (Array F sh a)

instance Unpack (Array F DIM1 a) (Int, Int, ForeignPtr a) where
 unpack (FArray (Z :. len) offset fptr) = (len, offset, fptr)
 repack _ (len, offset, fptr)           = FArray (Z :. len) offset fptr
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-- Window ---------------------------------------------------------------------
instance Storable a => Window F DIM1 a where
 window (Z :. start) sh' (FArray _ offset ptr)
        = FArray sh' (offset + start) ptr
 {-# INLINE window #-}
 {-# SPECIALIZE instance Window F DIM1 Int     #-}
 {-# SPECIALIZE instance Window F DIM1 Float   #-}
 {-# SPECIALIZE instance Window F DIM1 Double  #-}
 {-# SPECIALIZE instance Window F DIM1 Word8   #-}
 {-# SPECIALIZE instance Window F DIM1 Word16  #-}
 {-# SPECIALIZE instance Window F DIM1 Word32  #-}
 {-# SPECIALIZE instance Window F DIM1 Word64  #-}


-- Target ---------------------------------------------------------------------
instance Storable a => Target F a (Int, Int, ForeignPtr a) where
 data Buffer F a
        = FBuffer
        { -- | Starting position of data, in elements.
          _fBufferStart :: !Int

          -- | Length of buffer, in element.
        , _fBufferLen   :: !Int

          -- | Pointer to buffer data.
        , _fBufferFPtr  :: !(ForeignPtr a) }

 unsafeNewBuffer len
  = do  let (proxy :: a) = undefined
        ptr     <- mallocBytes (sizeOf proxy * len)
        _       <- peek ptr  `asTypeOf` return proxy
        
        fptr    <- newForeignPtr finalizerFree ptr
        return  $ FBuffer 0 len fptr
 {-# INLINE unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (FBuffer start _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) (start + ix) x
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer (FBuffer start len fptr) bump
  =  withForeignPtr fptr $ \ptr 
  -> do let (proxy :: a) = undefined
        let len'         = len + bump
        let bytesLen'    = sizeOf proxy * len'
        let bytesStart   = sizeOf proxy * start

        ptr'            <- mallocBytes bytesLen'
        copyBytes ptr' (plusPtr ptr bytesStart) bytesLen'

        fptr'   <- newForeignPtr finalizerFree ptr'
        return  $ FBuffer 0 len' fptr'
 {-# INLINE unsafeGrowBuffer #-}

 unsafeFreezeBuffer !sh (FBuffer start _len fptr)
  =     return  $ FArray sh start fptr
 {-# INLINE unsafeFreezeBuffer #-}

 unsafeSliceBuffer start' len (FBuffer start _len fptr)
  =     return  $ FBuffer (start + start') len fptr
 {-# INLINE unsafeSliceBuffer #-}

 touchBuffer (FBuffer _ _ fptr)
  = touchForeignPtr fptr
 {-# INLINE touchBuffer #-}

 {-# SPECIALIZE instance Target F Int    (Int, Int, ForeignPtr Int)    #-}
 {-# SPECIALIZE instance Target F Float  (Int, Int, ForeignPtr Float)  #-}
 {-# SPECIALIZE instance Target F Double (Int, Int, ForeignPtr Double) #-}
 {-# SPECIALIZE instance Target F Word8  (Int, Int, ForeignPtr Word8)  #-}
 {-# SPECIALIZE instance Target F Word16 (Int, Int, ForeignPtr Word16) #-}
 {-# SPECIALIZE instance Target F Word32 (Int, Int, ForeignPtr Word32) #-}
 {-# SPECIALIZE instance Target F Word64 (Int, Int, ForeignPtr Word64) #-}


instance Unpack (Buffer F a) (Int, Int, ForeignPtr a) where
 unpack (FBuffer len offset fptr) = (len, offset, fptr)
 repack _ (len, offset, fptr)     = FBuffer len offset fptr
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


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


-- Comparisons ----------------------------------------------------------------
-- TODO: do Haskell-level comparison for short vectors.
eqVectorF :: Vector F Word8 -> Vector F Word8 -> Bool
eqVectorF (FArray (Z :. len1) offset1 fptr1)
          (FArray (Z :. len2) offset2 fptr2)
 | len1 == len2
 =  unsafePerformIO
 $  withForeignPtr fptr1 $ \ptr1
 -> withForeignPtr fptr2 $ \ptr2
 -> do  r       <- BS.memcmp (plusPtr ptr1 offset1) 
                             (plusPtr ptr2 offset2) len1
        return  $ r == 0

 | otherwise
 = False

