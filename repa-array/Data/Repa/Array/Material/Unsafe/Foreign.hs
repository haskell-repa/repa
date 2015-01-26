{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Unsafe.Foreign
        ( U.F    (..)
        , Array  (..)
        , Buffer (..)
        , Window (..)

          -- * Conversions
        , fromForeignPtr,       toForeignPtr
        , fromByteString,       toByteString)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Checked
import Data.Repa.Array.Window
import Data.Repa.Array.Shape
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
import qualified Data.Repa.Array.Material.Safe.Base     as S
import qualified Data.Repa.Array.Material.Unsafe.Base   as U
import qualified Data.ByteString.Internal               as BS
#include "repa-stream.h"


------------------------------------------------------------------------------
-- | Unsafe Foreign arrays.
instance Repr U.F where
 type Safe   U.F  = S.F
 type Unsafe U.F  = U.F
 repr             = U.F
 {-# INLINE repr #-}


-- | Foreign arrays.
instance Repr S.F where
 type Safe   S.F  = S.F
 type Unsafe S.F  = U.F
 repr             = S.F
 {-# INLINE repr #-}


-------------------------------------------------------------------------------
-- | Unsafe Foreign arrays.
instance (Shape sh, Storable a) 
       => Bulk U.F sh a where

 data Array U.F sh a      = UFArray !sh !Int !(ForeignPtr a)
 extent (UFArray sh _ _)  = sh
 index  (UFArray sh offset fptr) ix
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (offset + toIndex sh ix)
 safe   arr             = SFArray (checked arr)
 unsafe arr             = arr
 {-# INLINE_ARRAY extent #-}
 {-# INLINE_ARRAY index  #-}
 {-# INLINE_ARRAY safe   #-}
 {-# INLINE_ARRAY unsafe #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Char    #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk U.F DIM1 Word64  #-}

deriving instance (Show sh, Show e) => Show (Array U.F sh e)


-------------------------------------------------------------------------------
-- | Foreign arrays.
instance (Shape sh, Storable a) 
       => Bulk S.F sh a where

 data Array S.F sh a       = SFArray !(Array (K U.F) sh a)
 extent (SFArray inner)    = extent inner
 index  (SFArray inner) ix = index inner ix
 safe   arr                = arr
 unsafe (SFArray inner)    = unchecked inner
 {-# INLINE_ARRAY extent #-}
 {-# INLINE_ARRAY index  #-}
 {-# INLINE_ARRAY safe   #-}
 {-# INLINE_ARRAY unsafe #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Char    #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk S.F DIM1 Word64  #-}

deriving instance (Show sh, Show e) => Show (Array S.F sh e)


-------------------------------------------------------------------------------
-- | Unpack Unsafe Foreign arrays.
instance Unpack (Array U.F DIM1 a) (Int, Int, ForeignPtr a) where
 unpack (UFArray (Z :. len) offset fptr) 
        = (len, offset, fptr)
 {-# INLINE_ARRAY unpack #-}

 repack _ (len, offset, fptr)
        = UFArray (Z :. len) offset fptr
 {-# INLINE_ARRAY repack #-}


-- | Unpack Foreign arrays.
instance Unpack (Array S.F DIM1 a)  (Int, Int, ForeignPtr a) where
 unpack (SFArray (KArray (UFArray (Z :. len) offset fptr))) 
        = (len, offset, fptr)
 {-# INLINE_ARRAY unpack #-}

 repack _ (len, offset, fptr) 
        = SFArray (KArray (UFArray (Z :. len) offset fptr))
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | Windowing Unsafe Foreign arrays.
instance Storable a 
      => Window U.F DIM1 a where
 window (Z :. start) sh' (UFArray _ offset ptr)
        = UFArray sh' (offset + start) ptr
 {-# INLINE_ARRAY window #-}
 {-# SPECIALIZE instance Window U.F DIM1 Char    #-}
 {-# SPECIALIZE instance Window U.F DIM1 Int     #-}
 {-# SPECIALIZE instance Window U.F DIM1 Float   #-}
 {-# SPECIALIZE instance Window U.F DIM1 Double  #-}
 {-# SPECIALIZE instance Window U.F DIM1 Word8   #-}
 {-# SPECIALIZE instance Window U.F DIM1 Word16  #-}
 {-# SPECIALIZE instance Window U.F DIM1 Word32  #-}
 {-# SPECIALIZE instance Window U.F DIM1 Word64  #-}


-------------------------------------------------------------------------------
-- | Unsafe Foreign buffers.
instance Storable a 
      => Target U.F a (Int, Int, ForeignPtr a) where
 data Buffer U.F a
        = UFBuffer 
                !Int            -- starting position of data, in elements.
                !Int            -- length of buffer, in elements.
                !(ForeignPtr a) -- element data.

 unsafeNewBuffer len
  = do  let (proxy :: a) = undefined
        ptr     <- mallocBytes (sizeOf proxy * len)
        _       <- peek ptr  `asTypeOf` return proxy
        
        fptr    <- newForeignPtr finalizerFree ptr
        return  $ UFBuffer 0 len fptr
 {-# INLINE_ARRAY unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (UFBuffer start _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) (start + ix) x
 {-# INLINE_ARRAY unsafeWriteBuffer #-}

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
 {-# INLINE_ARRAY unsafeGrowBuffer #-}

 unsafeFreezeBuffer !sh (UFBuffer start _len fptr)
  =     return  $ UFArray sh start fptr
 {-# INLINE_ARRAY unsafeFreezeBuffer #-}

 unsafeSliceBuffer start' len (UFBuffer start _len fptr)
  =     return  $ UFBuffer (start + start') len fptr
 {-# INLINE_ARRAY unsafeSliceBuffer #-}

 touchBuffer (UFBuffer _ _ fptr)
  = touchForeignPtr fptr
 {-# INLINE_ARRAY touchBuffer #-}

 {-# SPECIALIZE instance Target U.F Char   (Int, Int, ForeignPtr Char)   #-}
 {-# SPECIALIZE instance Target U.F Int    (Int, Int, ForeignPtr Int)    #-}
 {-# SPECIALIZE instance Target U.F Float  (Int, Int, ForeignPtr Float)  #-}
 {-# SPECIALIZE instance Target U.F Double (Int, Int, ForeignPtr Double) #-}
 {-# SPECIALIZE instance Target U.F Word8  (Int, Int, ForeignPtr Word8)  #-}
 {-# SPECIALIZE instance Target U.F Word16 (Int, Int, ForeignPtr Word16) #-}
 {-# SPECIALIZE instance Target U.F Word32 (Int, Int, ForeignPtr Word32) #-}
 {-# SPECIALIZE instance Target U.F Word64 (Int, Int, ForeignPtr Word64) #-}


-- | Unpack Unsafe Foreign buffers.
instance Unpack (Buffer U.F a) (Int, Int, ForeignPtr a) where
 unpack (UFBuffer start len fptr) = (start, len, fptr)
 repack _ (start, len, fptr)      = UFBuffer start len fptr
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr :: Shape sh => sh -> ForeignPtr a -> Array U.F sh a
fromForeignPtr !sh !fptr
        = UFArray sh 0 fptr
{-# INLINE_ARRAY fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array U.F sh a -> ForeignPtr a
toForeignPtr (UFArray _ _ fptr)
        = fptr
{-# INLINE_ARRAY toForeignPtr #-}


-- | O(1). Convert a foreign 'Vector' to a `ByteString`.
toByteString :: Vector U.F Word8 -> ByteString
toByteString (UFArray (Z :. len) offset fptr)
 = BS.PS fptr offset len
{-# INLINE_ARRAY toByteString #-}


-- | O(1). Convert a `ByteString` to an foreign `Vector`.
fromByteString :: ByteString -> Vector U.F Word8
fromByteString (BS.PS fptr offset len)
 = UFArray (Z :. len) offset fptr
{-# INLINE_ARRAY fromByteString #-}


-------------------------------------------------------------------------------
-- | Equality of Unsafe Foreign arrays.
instance Eq (Vector U.F Word8) where
 (==) (UFArray (Z :. len1) offset1 fptr1)
      (UFArray (Z :. len2) offset2 fptr2)
  | len1 == len2
  =  unsafePerformIO
  $  withForeignPtr fptr1 $ \ptr1
  -> withForeignPtr fptr2 $ \ptr2
  -> do  
        let loop_eq_VectorF ix
             | ix >= len1       = return True
             | otherwise        
             = do x1 <- peekElemOff ptr1 (offset1 + ix)
                  x2 <- peekElemOff ptr2 (offset2 + ix)
                  if x1 == x2
                   then loop_eq_VectorF (ix + 1)
                   else return False

        loop_eq_VectorF 0

  | otherwise = False
 {-# INLINE_ARRAY (==) #-}


-- | Equality of Unsafe Foreign arrays.
instance Eq (Vector U.F Char) where
 (==) (UFArray (Z :. len1) offset1 fptr1)
      (UFArray (Z :. len2) offset2 fptr2)
  |  len1 == len2
  =  unsafePerformIO
  $  withForeignPtr fptr1 $ \(ptr1 :: Ptr Char)
  -> withForeignPtr fptr2 $ \(ptr2 :: Ptr Char)
  -> do  
        let loop_eq_VectorF ix
             | ix >= len1       = return True
             | otherwise        
             = do x1 <- peekElemOff ptr1 (offset1 + ix)
                  x2 <- peekElemOff ptr2 (offset2 + ix)
                  if x1 == x2
                   then loop_eq_VectorF (ix + 1)
                   else return False

        loop_eq_VectorF 0

  | otherwise = False
 {-# INLINE_ARRAY (==) #-}
