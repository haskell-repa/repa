
module Data.Repa.Array.Auto.Format
        ( module Data.Repa.Convert.Format
        
        , packFormat
        , packsFixedFormat

        , unpackFormat
        , unpacksFixedFormat)
where
import Data.Repa.Array.Auto.Base                        as A
import Data.Repa.Array.Generic.Convert                  as A
import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Foreign       as A
import qualified Data.Repa.Array.Internals.Target       as A
import qualified Data.Repa.Array.Internals.Bulk         as A
import qualified Data.Repa.Convert.Format               as C
import Data.Repa.Convert.Format

import Foreign.ForeignPtr
import Foreign.Ptr

import System.IO.Unsafe
import Control.Monad
import Data.Word

import qualified Data.Vector.Storable.Mutable   as SM
#include "repa-array.h"


---------------------------------------------------------------------------------------------------
-- | Pack a value into a buffer using the given format.
packFormat
        :: C.Packable format
        => format                       -- ^ Format for the value.
        -> Value format                 -- ^ Source value.
        -> Maybe (Array Word8)          -- ^ Packed binary data.

packFormat !format !v
 | Just lenBytes    <- packedSize format v
 = unsafePerformIO
 $ do   
        -- The 'lenBytes' we get above is an upper bound on the
        -- number of used by the serialised data.
        buf@(A.FBuffer mvec) :: A.Buffer A.F Word8
                <- A.unsafeNewBuffer (A.Foreign lenBytes)

        let (fptr, oStart, _)   = SM.unsafeToForeignPtr mvec 

        -- When we pack the data into the buffer we get the number
        -- of bytes actually used, then slice the original buffer
        -- down to this size.
        withForeignPtr fptr $ \ptr
         -> C.pack (plusPtr ptr oStart) format v
         $  \len -> do
                buf'    <- A.unsafeSliceBuffer 0 len buf
                arr     <- A.unsafeFreezeBuffer buf'
                return  $ Just $ A.convert arr

 | otherwise = Nothing
{-# INLINE_ARRAY packFormat #-}


-- | Pack an array of fixed-length elements to a buffer using the given format.
packsFixedFormat
        :: (C.Packable format, A.Bulk A.A (C.Value format))
        => format                       -- ^ Fixed length format for each element.
        -> Array (C.Value format)       -- ^ Source elements.
        -> Maybe (Array Word8)          -- ^ Packed binary data.

packsFixedFormat !format !arrElems
 | Just rowSize <- C.fixedSize format
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
                     = C.pack (plusPtr ptr ixDst) format (A.index arrElems ixSrc)
                     $ \oElem -> loop (ixSrc + 1) (ixDst + oElem)

                mFinal <- loop 0 0
                case mFinal of
                 Nothing       -> return Nothing
                 Just _        -> liftM (Just . A.convert) $ A.unsafeFreezeBuffer buf

 | otherwise
 = Nothing
{-# INLINE_ARRAY packsFixedFormat #-}


---------------------------------------------------------------------------------------------------
-- | Unpack a value from a buffer using the given format.
unpackFormat
        :: C.Packable format
        => format                       -- ^ Format for the value.
        -> Array Word8                  -- ^ Packed binary data.
        -> Maybe (C.Value format)       -- ^ Unpacked value.

unpackFormat !format !arrBytes
 | lenBytes       <- A.length arrBytes
 = unsafePerformIO
 $ let  (oStart, _, fptr :: ForeignPtr Word8) 
         = A.toForeignPtr $ A.convert arrBytes
   in   withForeignPtr fptr $ \ptr_
         -> C.unpack (plusPtr ptr_ oStart) lenBytes format 
          $ \(v, _) -> return (Just v)

 | otherwise = Nothing
{-# INLINE_ARRAY unpackFormat #-}


-- | Unpack an array of elements from a buffer
--   using the given fixed length format.
unpacksFixedFormat
        :: (Packable format, A.Target A.A (Value format))
        => format                         -- ^ Fixed length format for each element.
        -> Array Word8                    -- ^ Packed binary data.
        -> Maybe (Array (Value format))   -- ^ Unpacked elements.

unpacksFixedFormat !format !arrBytes
 | lenBytes       <- A.length arrBytes
 , Just lenElems  <- fieldCount format
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
                     = C.unpack (plusPtr ptr ixSrc) (lenBytes - ixSrc) format 
                     $ \(value, oElem) 
                     -> do A.unsafeWriteBuffer buf ixDst value
                           loop (ixSrc + oElem) (ixDst + 1)

                mFinal <- loop 0 0
                case mFinal of
                 Nothing        -> return Nothing
                 Just _         -> liftM Just $ A.unsafeFreezeBuffer buf

 | otherwise = Nothing
{-# INLINE_ARRAY unpacksFixedFormat #-}

