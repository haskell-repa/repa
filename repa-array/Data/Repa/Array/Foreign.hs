
module Data.Repa.Array.Foreign
        ( F, Array (..), Buffer (..)
        , fromForeignPtr, toForeignPtr
        , eqVectorF)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Index
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
--        | not $ inShapeRange zeroDim sh ix                    -- TODO: indexing
--        = error "repa-bulk.index[F]: out of range"

        | otherwise
        = unsafeInlineIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (offset + toIndex sh ix)
 {-# INLINE index #-}


-- Window ---------------------------------------------------------------------
instance Storable a 
      => Window F DIM1 a where
 window (Z :. start) sh' (FArray _ offset ptr)
        = FArray sh' (offset + start) ptr
 {-# INLINE window #-}


-- Target ---------------------------------------------------------------------
instance Storable a => Target F a where
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


