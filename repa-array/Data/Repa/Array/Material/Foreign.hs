
module Data.Repa.Array.Material.Foreign
        ( F      (..)
        , Name   (..)
        , Array  (..)
        , Buffer (..)

        -- * Conversions
        , unsafeCast
        , fromForeignPtr,       toForeignPtr
        , fromStorableVector,   toStorableVector
        , fromByteString,       toByteString

        -- * Binary Ingestion
        , module Data.Repa.Binary.Format
        , Packable
        , packForeign
        , unpackForeign)
where
import Data.Repa.Array.Delayed                  as A
import Data.Repa.Array.Window                   as A
import Data.Repa.Array.Index                    as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Material.Auto            as A

import Data.Repa.Fusion.Unpack

import Data.Repa.Binary.Packable
import Data.Repa.Binary.Format

import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr

import Control.Monad
import Control.Monad.Primitive
import System.IO.Unsafe
import Data.Word

import Data.ByteString                          (ByteString)
import qualified Data.ByteString.Internal       as BS
import qualified Data.Vector.Storable.Mutable   as SM
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as M


#include "repa-array.h"


-- | Layout for dense Foreign arrays.
--
--   UNSAFE: Indexing into raw material arrays is not bounds checked.
--   You may want to wrap this with a Checked layout as well.
--
data F = Foreign { foreignLength :: Int }
  deriving (Show, Eq)


------------------------------------------------------------------------------
-- | Foreign arrays.
instance Layout F where
  data Name  F            = F
  type Index F            = Int
  name                    = F
  create F len            = Foreign len
  extent (Foreign len)    = len
  toIndex   _ ix          = ix
  fromIndex _ ix          = ix
  {-# INLINE_ARRAY name      #-}
  {-# INLINE_ARRAY create    #-}
  {-# INLINE_ARRAY extent    #-}
  {-# INLINE_ARRAY toIndex   #-}
  {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name F)
deriving instance Show (Name F)


-------------------------------------------------------------------------------
-- | Foreign arrays.
instance Storable a => Bulk F a where
  data Array F a      = FArray !(S.Vector a)
  layout (FArray v)   = Foreign (S.length v)
  index  (FArray v) i = S.unsafeIndex v i
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

deriving instance (S.Storable a, Show a) => Show (Array F a)

instance Unpack (Array F a) (S.Vector a) where
 unpack (FArray v) = v
 repack _ v        = FArray v
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | Windowing Foreign arrays.
instance Storable a => Windowable F a where
  window st len (FArray vec)
         = FArray (S.slice st len vec)
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
  data Buffer s F a = FBuffer !(M.MVector s a)

  unsafeNewBuffer    (Foreign n)        = FBuffer `liftM` M.unsafeNew n
  unsafeReadBuffer   (FBuffer mv) i     = M.unsafeRead mv i
  unsafeWriteBuffer  (FBuffer mv) i a   = M.unsafeWrite mv i a
  unsafeGrowBuffer   (FBuffer mv) x     = FBuffer `liftM` M.unsafeGrow mv x
  unsafeThawBuffer   (FArray v)         = FBuffer `liftM` S.unsafeThaw v
  unsafeFreezeBuffer (FBuffer mv)       = FArray  `liftM` S.unsafeFreeze mv
  unsafeSliceBuffer i n (FBuffer mv)    = return $ FBuffer (M.unsafeSlice i n mv)
  touchBuffer (FBuffer (M.MVector _ p)) = unsafePrimToPrim $ touchForeignPtr p
  bufferLayout (FBuffer mv)             = Foreign $ M.length mv
  {-# INLINE unsafeNewBuffer    #-}
  {-# INLINE unsafeWriteBuffer  #-}
  {-# INLINE unsafeReadBuffer   #-}
  {-# INLINE unsafeGrowBuffer   #-}
  {-# INLINE unsafeThawBuffer   #-}
  {-# INLINE unsafeFreezeBuffer #-}
  {-# INLINE unsafeSliceBuffer  #-}
  {-# INLINE touchBuffer        #-}
  {-# INLINE bufferLayout       #-}


-- | Unpack Foreign buffers
instance Unpack (Buffer s F a) (M.MVector s a) where
 unpack (FBuffer mv)  = mv
 repack _ mv          = FBuffer mv
 {-# INLINE_ARRAY unpack #-}
 {-# INLINE_ARRAY repack #-}


-------------------------------------------------------------------------------
-- | O(1). Cast a foreign array from one element type to another.
unsafeCast 
        :: (Storable a, Storable b)
        => Array F a -> Array F b
unsafeCast (FArray vec) 
        = FArray $ S.unsafeCast vec
{-# INLINE_ARRAY unsafeCast #-}


-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr :: Storable a => Int -> ForeignPtr a -> Array F a
fromForeignPtr n p = FArray $ S.unsafeFromForeignPtr p 0 n
{-# INLINE_ARRAY fromForeignPtr #-}


-- | O(1). Unwrap a `ForeignPtr` from an array.
toForeignPtr :: Storable a => Array F a -> (Int, Int, ForeignPtr a)
toForeignPtr (FArray (S.unsafeToForeignPtr -> (p,i,n))) = (i,n,p)
{-# INLINE_ARRAY toForeignPtr #-}


-- | O(1). Convert a foreign array to a storable `Vector`.
toStorableVector :: Array F a -> S.Vector a
toStorableVector (FArray vec) = vec
{-# INLINE_ARRAY toStorableVector #-}


-- | O(1). Convert a storable `Vector` to a foreign `Array`
fromStorableVector :: S.Vector a -> Array F a 
fromStorableVector vec = FArray vec
{-# INLINE_ARRAY fromStorableVector #-}


-- | O(1). Convert a foreign 'Vector' to a `ByteString`.
toByteString :: Array F Word8 -> ByteString
toByteString (FArray (S.unsafeToForeignPtr -> (p,i,n)))
 = BS.PS p i n
{-# INLINE_ARRAY toByteString #-}


-- | O(1). Convert a `ByteString` to an foreign `Array`.
fromByteString :: ByteString -> Array F Word8
fromByteString (BS.PS p i n)
 = FArray (S.unsafeFromForeignPtr p i n)
{-# INLINE_ARRAY fromByteString #-}


instance (Eq a, Storable a) => Eq (Array F a) where
  (FArray a1) == (FArray a2) = a1 == a2
  {-# INLINE_ARRAY (==) #-}


---------------------------------------------------------------------------------------------------
-- | Pack some array elements into a foreign buffer using the given binary
--   format.
packForeign    
        :: (Packable format, Bulk l (Value format), Index l ~ Int)
        => format                       -- ^ Binary format for each element.
        -> Array l (Value format)       -- ^ Source elements.
        -> Maybe (Array F Word8)        -- ^ Packed binary data.

packForeign !format !arrElems
 | Just rowSize <- fixedSize format
 , lenElems     <- A.length arrElems
 , lenBytes     <- rowSize * lenElems
 
 = unsafePerformIO
 $ do   
        buf@(FBuffer mvec)     
                <- unsafeNewBuffer (Foreign lenBytes)
        let (fptr, oStart, _) = SM.unsafeToForeignPtr mvec 

        withForeignPtr fptr $ \ptr_
         -> do  let ptr = plusPtr ptr_ oStart

                let loop !ixSrc !ixDst
                     | ixSrc >= lenElems
                     = return True

                     | otherwise
                     = Data.Repa.Binary.Packable.pack   
                                (plusPtr ptr ixDst) format (A.index arrElems ixSrc)
                     $ \oElem   -> loop (ixSrc + 1) (ixDst + oElem)

                mFinal <- loop 0 0
                case mFinal of
                 False       -> return Nothing
                 True        -> liftM Just $ unsafeFreezeBuffer buf

 | otherwise
 = Nothing
{-# INLINE_ARRAY packForeign #-}


---------------------------------------------------------------------------------------------------
-- | Unpack an array of elements from a foreign buffer into their standard
--   in-memory representation.
--
--   The binary format of the elements in the buffer is given by the
--   format specififier, while the in-memory representation is chosen
--   automagically based on the type of the elements.
--
unpackForeign 
        :: (Packable format, Target A (Value format))
        => format                         -- ^ Binary format for each element.
        -> Array F Word8                  -- ^ Packed binary data.
        -> Maybe (Array A (Value format)) -- ^ Unpacked elements.

unpackForeign !format !arrBytes
 | Just rowSize <- fixedSize format
 , lenBytes     <- A.length arrBytes
 , lenBytes `mod` rowSize == 0
 , lenElems     <- lenBytes `div` rowSize
 = unsafePerformIO
 $ do   
        let (oStart, _, fptr) 
                = toForeignPtr arrBytes

        withForeignPtr fptr $ \ptr_
         -> do  let ptr =  plusPtr ptr_ oStart
                buf     <- unsafeNewBuffer (Auto lenElems)

                let loop !ixSrc !ixDst 
                     | ixDst >= lenElems
                     = return $ Just ixSrc

                     | otherwise
                     = Data.Repa.Binary.Packable.unpack 
                                (plusPtr ptr ixSrc) format 
                     $ \(value, oElem) -> do
                        unsafeWriteBuffer buf ixDst value
                        loop (ixSrc + oElem) (ixDst + 1)

                mFinal <- loop 0 0
                case mFinal of
                 Nothing        -> return Nothing
                 Just _         -> liftM Just $ unsafeFreezeBuffer buf

 | otherwise
 = Nothing
{-# INLINE_ARRAY unpackForeign #-}

