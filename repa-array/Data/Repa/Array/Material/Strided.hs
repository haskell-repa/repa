
module Data.Repa.Array.Material.Strided
        ( S      (..)
        , Name   (..)
        , Array  (..)

        -- * Conversions
        , unsafeCast
        , fromForeignPtr,       toForeignPtr)
where
import Data.Repa.Array.Meta.Window
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Layout
import Data.Repa.Fusion.Unpack
import Data.Word
import qualified Foreign.Storable               as S
import qualified Foreign.ForeignPtr             as F
import qualified Data.ByteString.Internal       as BS
#include "repa-array.h"


-- | Layout for Foreign Strided arrays.
--
--   UNSAFE: indexing into foreign strided arrays is not bounds checked.
--   You may want to wrap this with a Checked layout as well.
--
data S  = Strided 
        { stridedLength :: !Int }
        deriving (Show, Eq)


-------------------------------------------------------------------------------
instance Layout S where
  data Name S           = S
  type Index S          = Int

  name                  = S
  create S len          = Strided len
  extent (Strided len)  = len
  toIndex   _ ix        = ix
  fromIndex _ ix        = ix
  {-# INLINE_ARRAY name      #-}
  {-# INLINE_ARRAY create    #-}
  {-# INLINE_ARRAY extent    #-}
  {-# INLINE_ARRAY toIndex   #-}
  {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name S)
deriving instance Show (Name S)


-------------------------------------------------------------------------------
instance S.Storable a => Bulk S a where
  data Array S a         
        = SArray
        { sArrayStartBytes   :: !Int
        , sArrayStrideBytes  :: !Int
        , sArrayLenElems     :: !Int 
        , sArrayPtr          :: !(F.ForeignPtr a) }

  layout (SArray _ _ len _)  
   = Strided len

  index  (SArray start stride len fptr) ix
   = BS.inlinePerformIO
         $ F.withForeignPtr fptr
         $ \ptr -> S.peekByteOff ptr 
                      (start + (toIndex (Strided len) ix) * stride)
  {-# INLINE_ARRAY layout #-}
  {-# INLINE_ARRAY index  #-}
  {-# SPECIALIZE instance Bulk S Char    #-}
  {-# SPECIALIZE instance Bulk S Int     #-}
  {-# SPECIALIZE instance Bulk S Float   #-}
  {-# SPECIALIZE instance Bulk S Double  #-}
  {-# SPECIALIZE instance Bulk S Word8   #-}
  {-# SPECIALIZE instance Bulk S Word16  #-}
  {-# SPECIALIZE instance Bulk S Word32  #-}
  {-# SPECIALIZE instance Bulk S Word64  #-}


deriving instance (S.Storable a, Show a) => Show (Array S a)


instance Unpack (Array S a) (Int, Int, Int, F.ForeignPtr a) where
  unpack   (SArray start stride len fptr)  = (start, stride, len, fptr)
  repack _ (start, stride, len, fptr)      = (SArray start stride len fptr)
  {-# INLINE unpack #-}
  {-# INLINE repack #-}


-------------------------------------------------------------------------------
instance S.Storable a => Windowable S a where
  window startElems' lenElems' 
         (SArray startBytes strideBytes _lenElems fptr)
   = let lenElem = S.sizeOf (undefined :: a)
     in  SArray (startBytes + (lenElem * startElems'))
                strideBytes lenElems' fptr
  {-# INLINE_ARRAY window #-}
  {-# SPECIALIZE instance Windowable S Char    #-}
  {-# SPECIALIZE instance Windowable S Int     #-}
  {-# SPECIALIZE instance Windowable S Float   #-}
  {-# SPECIALIZE instance Windowable S Double  #-}
  {-# SPECIALIZE instance Windowable S Word8   #-}
  {-# SPECIALIZE instance Windowable S Word16  #-}
  {-# SPECIALIZE instance Windowable S Word32  #-}
  {-# SPECIALIZE instance Windowable S Word64  #-}


-------------------------------------------------------------------------------
-- | O(1). Cast a foreign array from one element type to another.
unsafeCast
        :: (S.Storable a, S.Storable b)
        => Array S a -> Array S b
unsafeCast (SArray startBytes strideBytes lenElems fptr)
        =  (SArray startBytes strideBytes lenElems $ F.castForeignPtr fptr)


-- | O(1). Wrap a `ForeignPtr` as a strided array.
fromForeignPtr 
        :: Int            -- ^ Starting position in bytes.
        -> Int            -- ^ Stride to get to next element, in bytes.
        -> Int            -- ^ Length of array in elements.
        -> F.ForeignPtr a -- ^ `ForeignPtr` holding the data.
        -> Array S a

fromForeignPtr startBytes strideBytes lenElems fptr
      = SArray startBytes strideBytes lenElems fptr
{-# INLINE_ARRAY fromForeignPtr #-}


-- | O(1). Unwrap a `ForeignPtr` from a strided array.
toForeignPtr
        :: Array S a
        -> (Int, Int, Int, F.ForeignPtr a)
toForeignPtr (SArray startBytes strideBytes lenElems fptr)
        = (startBytes, strideBytes, lenElems, fptr)
{-# INLINE_ARRAY toForeignPtr #-}

