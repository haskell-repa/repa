{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Foreign
        ( F      (..)
        , Name   (..)
        , Array  (..)
        , Buffer (..)

          -- * Conversions
        , fromForeignPtr,       toForeignPtr
        , fromByteString,       toByteString)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import System.IO.Unsafe
import Data.Repa.Fusion.Unpack
import Data.ByteString                                  (ByteString)
import qualified Foreign.ForeignPtr.Unsafe              as Unsafe
import qualified Data.ByteString.Internal               as BS
#include "repa-array.h"


-- | Layout for Foreign arrays.
--
--   UNSAFE: Indexing into raw material arrays is not bounds checked.
--   You may want to wrap this with a Checked layout as well.
--
data F  = Foreign 
        { foreignLength :: Int}

deriving instance Eq F
deriving instance Show F


------------------------------------------------------------------------------
-- | Foreign arrays.
instance Layout F where
 data Name  F            = F
 type Index F            = Int
 create F len            = Foreign len
 extent (Foreign len)    = len
 toIndex   _ ix          = ix
 fromIndex _ ix          = ix
 {-# INLINE_ARRAY create    #-}
 {-# INLINE_ARRAY extent    #-}
 {-# INLINE_ARRAY toIndex   #-}
 {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name F)
deriving instance Show (Name F)


-------------------------------------------------------------------------------
-- | Foreign arrays.
instance Storable a => Bulk F a where
 data Array F  a          = FArray !Int !Int !(ForeignPtr a)
 layout (FArray _  len _) = Foreign len
 index  (FArray st len fptr) ix
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (st + toIndex (Foreign len) ix)
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}
 {-# SPECIALIZE instance Bulk F Char    #-}
 {-# SPECIALIZE instance Bulk F Int     #-}
 {-# SPECIALIZE instance Bulk F Float   #-}
 {-# SPECIALIZE instance Bulk F Double  #-}
 {-# SPECIALIZE instance Bulk F Word8   #-}
 {-# SPECIALIZE instance Bulk F Word16  #-}
 {-# SPECIALIZE instance Bulk F Word32  #-}
 {-# SPECIALIZE instance Bulk F Word64  #-}

deriving instance Show a => Show (Array F a)


-- | Foreign arrays
instance Unpack (Array F a) (Int, Int, ForeignPtr a) where
 unpack (FArray st len fptr)    = (st, len, fptr)
 repack _ (st, len, fptr)       = FArray st len fptr
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | Windowing Foreign arrays.
instance Storable a => Windowable F a where
 window st' len' (FArray st _len fptr)
        = FArray (st + st') len' fptr
 {-# INLINE_ARRAY window #-}
 {-# SPECIALIZE instance Windowable F Char    #-}
 {-# SPECIALIZE instance Windowable F Int     #-}
 {-# SPECIALIZE instance Windowable F Float   #-}
 {-# SPECIALIZE instance Windowable F Double  #-}
 {-# SPECIALIZE instance Windowable F Word8   #-}
 {-# SPECIALIZE instance Windowable F Word16  #-}
 {-# SPECIALIZE instance Windowable F Word32  #-}
 {-# SPECIALIZE instance Windowable F Word64  #-}


-------------------------------------------------------------------------------
-- | Foreign buffers
instance Storable a => Target F a where
 data Buffer F a
        = FBuffer 
                !Int            -- starting position of data, in elements.
                !Int            -- length of buffer, in elements.
                !(ForeignPtr a) -- element data.

 unsafeNewBuffer (Foreign len)
  = do  let (proxy :: a) = undefined
        ptr     <- mallocBytes (sizeOf proxy * len)
        _       <- peek ptr  `asTypeOf` return proxy
        
        fptr    <- newForeignPtr finalizerFree ptr
        return  $ FBuffer 0 len fptr
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (FBuffer start _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) (start + ix) x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

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
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer (FBuffer start len fptr)
  =     return  $ FArray start len fptr
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeSliceBuffer start' len' (FBuffer start _len fptr)
  =     return  $ FBuffer (start + start') len' fptr
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (FBuffer _ _ fptr)
  = touchForeignPtr fptr
 {-# INLINE_ARRAY touchBuffer #-}

 {-# SPECIALIZE instance Target F Char   #-}
 {-# SPECIALIZE instance Target F Int    #-}
 {-# SPECIALIZE instance Target F Float  #-}
 {-# SPECIALIZE instance Target F Double #-}
 {-# SPECIALIZE instance Target F Word8  #-}
 {-# SPECIALIZE instance Target F Word16 #-}
 {-# SPECIALIZE instance Target F Word32 #-}
 {-# SPECIALIZE instance Target F Word64 #-}


-- | Unpack Foreign buffers
instance Unpack (Buffer F a) (Int, Int, ForeignPtr a) where
 unpack (FBuffer start len fptr)  = (start, len, fptr)
 repack _ (start, len, fptr)      = FBuffer start len fptr
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr :: Int -> ForeignPtr a -> Array F a
fromForeignPtr !len !fptr
        = FArray 0 len fptr
{-# INLINE_ARRAY fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array F a -> ForeignPtr a
toForeignPtr (FArray _ _ fptr)
        = fptr
{-# INLINE_ARRAY toForeignPtr #-}


-- | O(1). Convert a foreign 'Vector' to a `ByteString`.
toByteString :: Array F Word8 -> ByteString
toByteString (FArray start len fptr)
 = BS.PS fptr start len
{-# INLINE_ARRAY toByteString #-}


-- | O(1). Convert a `ByteString` to an foreign `Vector`.
fromByteString :: ByteString -> Array F Word8
fromByteString (BS.PS fptr start len)
 = FArray start len fptr
{-# INLINE_ARRAY fromByteString #-}


-------------------------------------------------------------------------------
-- | Equality of Unsafe Foreign arrays.
instance Eq (Array F Word8) where
 (==) (FArray start1 len1 fptr1)
      (FArray start2 len2 fptr2)
  | len1 == len2
  =  unsafePerformIO
  $  withForeignPtr fptr1 $ \ptr1
  -> withForeignPtr fptr2 $ \ptr2
  -> do  
        let loop_eq_ArrayF ix
             | ix >= len1       = return True
             | otherwise        
             = do x1 <- peekElemOff ptr1 (start1 + ix)
                  x2 <- peekElemOff ptr2 (start2 + ix)
                  if x1 == x2
                   then loop_eq_ArrayF (ix + 1)
                   else return False

        loop_eq_ArrayF 0

  | otherwise = False
 {-# INLINE_ARRAY (==) #-}


-- | Equality of Unsafe Foreign arrays.
instance Eq (Array F Char) where
 (==) (FArray start1 len1 fptr1)
      (FArray start2 len2 fptr2)
  |  len1 == len2
  =  unsafePerformIO
  $  withForeignPtr fptr1 $ \(ptr1 :: Ptr Char)
  -> withForeignPtr fptr2 $ \(ptr2 :: Ptr Char)
  -> do  
        let loop_eq_ArrayF ix
             | ix >= len1       = return True
             | otherwise        
             = do x1 <- peekElemOff ptr1 (start1 + ix)
                  x2 <- peekElemOff ptr2 (start2 + ix)
                  if x1 == x2
                   then loop_eq_ArrayF (ix + 1)
                   else return False

        loop_eq_ArrayF 0

  | otherwise = False
 {-# INLINE_ARRAY (==) #-}

