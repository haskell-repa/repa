
module Data.Repa.Array.Material.AutoUnpack
        ( module Data.Repa.Convert.Format
        , packForeign
        , unpackForeign)
where
import Data.Repa.Array.Material.Auto            as A
import Data.Repa.Array.Material.Foreign         as A
import Data.Repa.Array.Index                    as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Convert.Format

import Foreign.ForeignPtr
import Foreign.Ptr

import System.IO.Unsafe
import Control.Monad
import Data.Word

import qualified Data.Vector.Storable.Mutable   as SM
#include "repa-array.h"


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
                     = Data.Repa.Convert.Format.pack   
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
                     = Data.Repa.Convert.Format.unpack 
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
