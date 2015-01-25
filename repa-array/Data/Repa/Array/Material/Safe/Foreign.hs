{-# OPTIONS -fno-warn-orphans #-}
module Data.Repa.Array.Material.Safe.Foreign
        ( S.F    (..)
        , Array  (..)
        , Buffer (..)
        , fromForeignPtr, toForeignPtr
        , fromByteString, toByteString)
where
import qualified  Data.Repa.Array.Material.Unsafe.Foreign  as UF
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Checked
import Data.Repa.Array.Shape
import Data.Repa.Eval.Array
import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Internals.Bulk
import Foreign.ForeignPtr
import Data.Word
import Data.ByteString                          (ByteString)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.Repa.Array.Material.Unsafe.Foreign          (Array(..))
import qualified Data.Repa.Array.Material.Safe.Base     as S
import qualified Data.Repa.Array.Material.Unsafe.Base   as U
import qualified Foreign.ForeignPtr.Unsafe              as Unsafe


-------------------------------------------------------------------------------
-- | Foreign windows.
instance Storable a 
      => Window S.F DIM1 a where
 window (Z :. start) sh' (SFArray (KArray (UFArray _ offset ptr)))
        = SFArray (KArray (UFArray sh' (offset + start) ptr))
 {-# INLINE window #-}
 {-# SPECIALIZE instance Window S.F DIM1 Char    #-}
 {-# SPECIALIZE instance Window S.F DIM1 Int     #-}
 {-# SPECIALIZE instance Window S.F DIM1 Float   #-}
 {-# SPECIALIZE instance Window S.F DIM1 Double  #-}
 {-# SPECIALIZE instance Window S.F DIM1 Word8   #-}
 {-# SPECIALIZE instance Window S.F DIM1 Word16  #-}
 {-# SPECIALIZE instance Window S.F DIM1 Word32  #-}
 {-# SPECIALIZE instance Window S.F DIM1 Word64  #-}


-------------------------------------------------------------------------------
-- | Foreign buffers.
instance Storable a 
      => Target S.F a (Int, Int, ForeignPtr a) where
 data Buffer S.F a
        = SFBuffer
                !Int            -- Starting position of data, in elements.
                !Int            -- Length of buffer, in elements.
                !(ForeignPtr a) -- element data.

 unsafeNewBuffer len
  = do  let (proxy :: a) = undefined
        ptr     <- mallocBytes (sizeOf proxy * len)
        _       <- peek ptr  `asTypeOf` return proxy
        
        fptr    <- newForeignPtr finalizerFree ptr
        return  $ SFBuffer 0 len fptr
 {-# INLINE unsafeNewBuffer #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteBuffer (SFBuffer start _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) (start + ix) x
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer (SFBuffer start len fptr) bump
  =  withForeignPtr fptr $ \ptr 
  -> do let (proxy :: a) = undefined
        let len'         = len + bump
        let bytesLen'    = sizeOf proxy * len'
        let bytesStart   = sizeOf proxy * start

        ptr'            <- mallocBytes bytesLen'
        copyBytes ptr' (plusPtr ptr bytesStart) bytesLen'

        fptr'   <- newForeignPtr finalizerFree ptr'
        return  $ SFBuffer 0 len' fptr'
 {-# INLINE unsafeGrowBuffer #-}

 unsafeFreezeBuffer !sh (SFBuffer start _len fptr)
  =     return  $ SFArray (KArray (UFArray sh start fptr))
 {-# INLINE unsafeFreezeBuffer #-}

 unsafeSliceBuffer start' len (SFBuffer start _len fptr)
  =     return  $ SFBuffer (start + start') len fptr
 {-# INLINE unsafeSliceBuffer #-}

 touchBuffer (SFBuffer _ _ fptr)
  = touchForeignPtr fptr
 {-# INLINE touchBuffer #-}

 {-# SPECIALIZE instance Target S.F Int    (Int, Int, ForeignPtr Int)    #-}
 {-# SPECIALIZE instance Target S.F Float  (Int, Int, ForeignPtr Float)  #-}
 {-# SPECIALIZE instance Target S.F Double (Int, Int, ForeignPtr Double) #-}
 {-# SPECIALIZE instance Target S.F Word8  (Int, Int, ForeignPtr Word8)  #-}
 {-# SPECIALIZE instance Target S.F Word16 (Int, Int, ForeignPtr Word16) #-}
 {-# SPECIALIZE instance Target S.F Word32 (Int, Int, ForeignPtr Word32) #-}
 {-# SPECIALIZE instance Target S.F Word64 (Int, Int, ForeignPtr Word64) #-}


-- | Unpack foreign buffers.
instance Unpack (Buffer S.F a) (Int, Int, ForeignPtr a) where
 unpack (SFBuffer start len fptr) = (start, len, fptr)
 repack _ (start, len, fptr)      = SFBuffer start len fptr
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-------------------------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr :: Shape sh => sh -> ForeignPtr a -> Array S.F sh a
fromForeignPtr !sh !fptr
        = SFArray $ checked $ UF.fromForeignPtr sh fptr
{-# INLINE fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array S.F sh a -> ForeignPtr a
toForeignPtr (SFArray arr)
        = UF.toForeignPtr $ unchecked arr
{-# INLINE toForeignPtr #-}


-- | O(1). Convert a `ByteString` to an foreign `Vector`.
fromByteString :: ByteString -> Vector S.F Word8
fromByteString bs
        = SFArray $ checked $ UF.fromByteString bs
{-# INLINE fromByteString #-}


-- | O(1). Convert a foreign 'Vector' to a `ByteString`.
toByteString :: Vector S.F Word8 -> ByteString
toByteString (SFArray vec)
        = UF.toByteString $ unchecked vec
{-# INLINE toByteString #-}


-------------------------------------------------------------------------------
-- | Equality of Foreign arrays.
instance Eq (Vector S.F Word8) where
 (==) (SFArray (KArray arr1)) (SFArray (KArray arr2)) = arr1 == arr2
 {-# INLINE (==) #-}


-- | Equality of Foreign arrays.
instance Eq (Vector S.F Char)  where
 (==) (SFArray (KArray arr1)) (SFArray (KArray arr2)) = arr1 == arr2
 {-# INLINE (==) #-}


