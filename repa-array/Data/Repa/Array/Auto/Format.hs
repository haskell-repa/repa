
module Data.Repa.Array.Auto.Format
        ( module Data.Repa.Convert.Format
        
          -- * Packing
        , packFormat
        , packsFixedFormat

          -- * Unpacking
        , unpackFormat
        , unpacksFixedFormat
        , unpacksLinesFormat)
where
import Data.Repa.Array.Auto.Base                        as A
import Data.Repa.Array.Generic.Convert                  as A
import qualified Data.Repa.Array.Generic                as AG
import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Foreign       as A
import qualified Data.Repa.Array.Material.Nested        as A
import qualified Data.Repa.Array.Internals.Target       as A
import qualified Data.Repa.Array.Internals.Bulk         as A
import qualified Data.Repa.Convert.Format               as C
import Data.Repa.Convert.Format

import Foreign.ForeignPtr
import Foreign.Ptr

import System.IO.Unsafe
import Control.Monad
import Data.Word
import Data.Char

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
         -> do  Just ptr' <- C.unsafeRunPacker (C.pack format v) 
                                               (plusPtr ptr oStart)
                let len   =  minusPtr ptr' ptr
                buf'      <- A.unsafeSliceBuffer 0 len buf
                arr       <- A.unsafeFreezeBuffer  buf'
                return    $ Just $ A.convert arr

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
         -> do  
                let loop !ixSrc !ptrDst
                     | ixSrc >= lenElems
                     = return $ Just ()

                     | otherwise
                     = do  let x        =  A.index arrElems ixSrc
                           Just ptrDst' <- C.unsafeRunPacker (C.pack format x) ptrDst
                           loop (ixSrc + 1) ptrDst'

                let ptr = plusPtr ptr_ oStart
                mFinal <- loop 0 ptr

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
         -> do  r <- C.unsafeRunUnpacker 
                        (C.unpack format) 
                        (plusPtr ptr_ oStart)
                        lenBytes (const False)
                case r of
                 Just (v, _) -> return $ Just v
                 _           -> return Nothing

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
 | lenBytes     <- A.length arrBytes
 , lenElems     <- fieldCount format
 = unsafePerformIO
 $ do   
        let (oStart, _, fptr :: ForeignPtr Word8) 
                = A.toForeignPtr $ A.convert arrBytes

        withForeignPtr fptr $ \ptr_
         -> do  buf        <- A.unsafeNewBuffer (A.Auto lenElems)

                let !ptr     = plusPtr ptr_ oStart 
                let !ptrEnd  = plusPtr ptr  lenBytes

                let loop !ptrSrc !ixDst 
                     | ptrSrc >= ptrEnd
                     = return $ Just ptrSrc

                     | otherwise
                     = do r <- C.unsafeRunUnpacker
                                 (C.unpack format)
                                 ptrSrc 
                                 (minusPtr ptrEnd ptrSrc)
                                 (const False)

                          case r of
                           Just (value, ptrSrc') 
                            -> do A.unsafeWriteBuffer buf ixDst value
                                  loop ptrSrc' (ixDst + 1)

                           Nothing -> return Nothing

                mFinal <- loop ptr 0
                case mFinal of
                 Nothing        -> return Nothing
                 Just _         -> liftM Just $ A.unsafeFreezeBuffer buf

 | otherwise = Nothing
{-# INLINE_ARRAY unpacksFixedFormat #-}


-- | Unpack an array containing elements in some format separated
--   by newline ('\n') characters. 
-- 
--   * Any '\r' characters in the array are filtered out before splitting
--     it into lines.
--   * If the value cannot be converted then this function just calls `error`.
unpacksLinesFormat
        :: forall format
        .  (Show format, Packable format, A.Target A.A (Value format))
        => format                       -- ^ Format for each element.
        -> Array Word8                  -- ^ Packed binary data.
        -> Array (Value format)         -- ^ Unpacked elements.

unpacksLinesFormat format arr8
 = do
        let !nl = fromIntegral $ ord '\n'
        let !nr = fromIntegral $ ord '\r'

        let rows8 :: AG.Array A.N (AG.Array A.F Word8)
                = A.trimEnds  (== nl)
                $ A.segmentOn (== nl)
                $ AG.filter A.F  (/= nr) arr8

        -- TODO: if we had a mapM function we could write to a IORef to signal
        --       that something didn't convert.
        let unpackRow :: AG.Array A.A Word8 -> Value format
            unpackRow arr
             = case unpackFormat format arr of
                Nothing 
                 -> error $ unlines
                  [ "repa-array.unpacksLinesFormat: conversion failed"
                  , "    format       = " ++ show format
                  , "    row as Word8 = " ++ show (A.toList arr)
                  , "    row as Char8 = " ++ show (map (chr . fromIntegral) $ A.toList arr) ]

                Just v  -> v
            {-# INLINE unpackRow #-}

        AG.mapS A.A (unpackRow . AG.convert A.A) rows8
{-# INLINE unpacksLinesFormat #-}

