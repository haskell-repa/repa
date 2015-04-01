
module Data.Repa.Array.Auto.Unpack
        ( module Data.Repa.Convert.Format
        , packForeign
        , unpackForeign)
where
import Data.Repa.Array.Auto.Base                        as A
import Data.Repa.Array.Generic.Convert                  as A
import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Foreign       as A
import qualified Data.Repa.Array.Internals.Target       as A
import qualified Data.Repa.Array.Internals.Bulk         as A
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
        :: (Packable format, A.Bulk A.A (Value format))
        => format                       -- ^ Binary format for each element.
        -> Array (Value format)         -- ^ Source elements.
        -> Maybe (Array Word8)          -- ^ Packed binary data.

packForeign !format !arrElems
 | Just rowSize <- fixedSize format
 , lenElems     <- A.length arrElems
 , lenBytes     <- rowSize * lenElems
 
 = unsafePerformIO
 $ do   
        buf@(A.FBuffer mvec) :: A.Buffer A.F Word8
                <- A.unsafeNewBuffer (A.Foreign lenBytes)

        let (fptr, oStart, _)   = SM.unsafeToForeignPtr mvec 

        withForeignPtr fptr $ \ptr_
         -> do  let ptr = plusPtr ptr_ oStart

                let loop !ixSrc !ixDst
                     | ixSrc >= lenElems
                     = return $ Just ()

                     | otherwise
                     = Data.Repa.Convert.Format.pack   
                                (plusPtr ptr ixDst) format (A.index arrElems ixSrc)
                     $ \oElem   -> loop (ixSrc + 1) (ixDst + oElem)

                mFinal <- loop 0 0
                case mFinal of
                 Nothing       -> return Nothing
                 Just _        -> liftM (Just . A.convert) $ A.unsafeFreezeBuffer buf

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
        :: (Packable format, A.Target A.A (Value format))
        => format                       -- ^ Binary format for each element.
        -> Array Word8                  -- ^ Packed binary data.
        -> Maybe (Array (Value format)) -- ^ Unpacked elements.

unpackForeign !format !arrBytes
 | Just rowSize <- fixedSize format
 , lenBytes     <- A.length arrBytes
 , lenBytes `mod` rowSize == 0
 , lenElems     <- lenBytes `div` rowSize
 = unsafePerformIO
 $ do   
        let (oStart, _, fptr :: ForeignPtr Word8) 
                = A.toForeignPtr $ A.convert arrBytes

        withForeignPtr fptr $ \ptr_
         -> do  let ptr =  plusPtr ptr_ oStart
                buf     <- A.unsafeNewBuffer (A.Auto lenElems)

                let loop !ixSrc !ixDst 
                     | ixDst >= lenElems
                     = return $ Just ixSrc

                     | otherwise
                     = Data.Repa.Convert.Format.unpack 
                                (plusPtr ptr ixSrc) (lenBytes - ixSrc) format 
                     $ \(value, oElem) -> do
                        A.unsafeWriteBuffer buf ixDst value
                        loop (ixSrc + oElem) (ixDst + 1)

                mFinal <- loop 0 0
                case mFinal of
                 Nothing        -> return Nothing
                 Just _         -> liftM Just $ A.unsafeFreezeBuffer buf

 | otherwise
 = Nothing
{-# INLINE_ARRAY unpackForeign #-}
