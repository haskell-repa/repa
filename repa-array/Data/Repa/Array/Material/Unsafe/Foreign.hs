
module Data.Repa.Array.Material.Unsafe.Foreign
        ( UF    (..),           F(..)
        , Array (..)
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
import qualified Foreign.ForeignPtr.Unsafe      as Unsafe
import Data.Repa.Fusion.Unpack
import Data.ByteString                          (ByteString)
import qualified Data.ByteString.Internal       as BS


-------------------------------------------------------------------------------
-- | Arrays represented as foreign buffers in the C heap.
data UF = UF

-- | Arrays represented as foreign buffers in the C heap.
data F  = F


------------------------------------------------------------------------------
-- | Unsafe Foreign arrays.
instance Repr UF where
 type Safe   UF  = F
 type Unsafe UF  = UF
 repr = UF
 {-# INLINE repr #-}

-- | Foreign arrays.
instance Repr F where
 type Safe F     = F
 type Unsafe F   = UF
 repr = F
 {-# INLINE repr #-}


-------------------------------------------------------------------------------
-- | Unsafe Foreign arrays.
instance (Shape sh, Storable a) 
       => Bulk UF sh a where

 data Array UF sh a       = UFArray !sh !Int !(ForeignPtr a)
 extent (UFArray sh _ _)  = sh
 index (UFArray sh offset fptr) ix
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr (offset + toIndex sh ix)
 safe   arr             = FArray (checked arr)
 unsafe arr             = arr
 {-# INLINE extent #-}
 {-# INLINE index  #-}
 {-# INLINE safe   #-}
 {-# INLINE unsafe #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Char    #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk UF DIM1 Word64  #-}

deriving instance (Show sh, Show e) => Show (Array UF sh e)


-- | Foreign arrays.
instance (Shape sh, Storable a) 
       => Bulk F sh a where

 data Array F sh a        = FArray !(Array (K UF) sh a)
 extent (FArray inner)    = extent inner
 index  (FArray inner) ix = index inner ix
 safe   arr               = arr
 unsafe (FArray inner)    = unchecked inner
 {-# INLINE extent #-}
 {-# INLINE index  #-}
 {-# INLINE safe   #-}
 {-# INLINE unsafe #-}
 {-# SPECIALIZE instance Bulk F DIM1 Char    #-}
 {-# SPECIALIZE instance Bulk F DIM1 Int     #-}
 {-# SPECIALIZE instance Bulk F DIM1 Float   #-}
 {-# SPECIALIZE instance Bulk F DIM1 Double  #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word8   #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word16  #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word32  #-}
 {-# SPECIALIZE instance Bulk F DIM1 Word64  #-}

deriving instance (Show sh, Show e) => Show (Array F sh e)


-- Unpack ---------------------------------------------------------------------
instance Unpack (Array UF DIM1 a) (Int, Int, ForeignPtr a) where
 unpack (UFArray (Z :. len) offset fptr) = (len, offset, fptr)
 repack _ (len, offset, fptr)            = UFArray (Z :. len) offset fptr


instance Unpack (Array F DIM1 a)  (Int, Int, ForeignPtr a) where
 unpack (FArray (KArray (UFArray (Z :. len) offset fptr))) = (len, offset, fptr)
 repack _ (len, offset, fptr) = FArray (KArray (UFArray (Z :. len) offset fptr))


-- Window ---------------------------------------------------------------------
instance Storable a 
      => Window UF DIM1 a where
 window (Z :. start) sh' (UFArray _ offset ptr)
        = UFArray sh' (offset + start) ptr
 {-# INLINE window #-}
 {-# SPECIALIZE instance Window UF DIM1 Char    #-}
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

 {-# SPECIALIZE instance Target UF Char   (Int, Int, ForeignPtr Char)   #-}
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


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr :: Shape sh => sh -> ForeignPtr a -> Array UF sh a
fromForeignPtr !sh !fptr
        = UFArray sh 0 fptr
{-# INLINE fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array UF sh a -> ForeignPtr a
toForeignPtr (UFArray _ _ fptr)
        = fptr
{-# INLINE toForeignPtr #-}


-- | O(1). Convert a foreign 'Vector' to a `ByteString`.
toByteString :: Vector UF Word8 -> ByteString
toByteString (UFArray (Z :. len) offset fptr)
 = BS.PS fptr offset len
{-# INLINE toByteString #-}


-- | O(1). Convert a `ByteString` to an foreign `Vector`.
fromByteString :: ByteString -> Vector UF Word8
fromByteString (BS.PS fptr offset len)
 = UFArray (Z :. len) offset fptr
{-# INLINE fromByteString #-}


-- Comparisons ----------------------------------------------------------------
instance Eq (Vector UF Word8) where
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
 {-# INLINE (==) #-}


instance Eq (Vector UF Char) where
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
 {-# INLINE (==) #-}
