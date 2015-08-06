
module Data.Repa.Array.Auto.Format
        ( module Data.Repa.Convert.Format
        , module Data.Repa.Convert.Formats
        
          -- * Packing
        , packFormat
        , packsFormat
        , packsFormatLn

          -- * Unpacking
        , unpackFormat
        , unpacksFormatLn
        , unpacksFormatFixed)
where
import Data.Repa.Array.Auto.Base                                as A
import Data.Repa.Array.Generic.Convert                          as A
import qualified Data.Repa.Array.Generic                        as AG
import qualified Data.Repa.Array.Material.Auto                  as A
import qualified Data.Repa.Array.Material.Foreign               as AF
import qualified Data.Repa.Array.Internals.Target               as A
import qualified Data.Repa.Array.Internals.Bulk                 as A
import qualified Data.Repa.Array.Internals.Operator.Reduce      as A
import qualified Data.Repa.Convert.Format                       as C
import Data.Repa.Convert.Format
import Data.Repa.Convert.Formats

import Foreign.ForeignPtr                                       as F
import Foreign.Storable                                         as F
import Foreign.Ptr                                              as F

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
        buf@(AF.FBuffer mvec) :: A.Buffer AF.F Word8
                <- A.unsafeNewBuffer (AF.Foreign lenBytes)

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


-- | Pack an array of values into a buffer using the given format.
--
--   * The bytes representing each value are concatenated together with no delimitor.
--
packsFormat
        :: (C.Packable format, Show format, A.Bulk A.A (Value format))
        => format                       -- ^ Format for each value
        -> Array (Value format)         -- ^ Source values.
        -> Maybe (Array Word8)          -- ^ Packed binary data.

packsFormat !format !arr
 = let  
        packRow v 
         = case packFormat format v of
            Nothing
             -> error $ unlines
             [ "repa-array.packsFormat: conversion failed"]

            Just arr8 -> arr8
        {-# INLINE packRow #-}

   in   Just $ AG.concat A.A $ AG.mapS A.N packRow arr
{-# INLINE_ARRAY packsFormat #-}


-- | Like `packsFormat`, but append a newline character after
--   every packed element.
--
--   * If a value cannot be converted then this function just returns `error`.
--
packsFormatLn
        :: (C.Packable format, A.Bulk A.A (Value format))
        => format                       -- ^ Format for each value
        -> Array (Value format)         -- ^ Source values.
        -> Maybe (Array Word8)          -- ^ Packed binary data.

packsFormatLn !format !arr
 = let  
        !arrNL 
         = A.fromList A.A [fromIntegral $ ord '\n']                     -- TODO: kill lists

        packRow v 
         = case packFormat format v of
            Nothing
             -> error $ unlines
             [ "repa-array.packsFormat: conversion failed"]

            Just arr8 -> AG.concat A.A $ A.fromList A.A [ arr8, arrNL ]
        {-# INLINE packRow #-}

   in   Just $ AG.concat A.A $ AG.mapS A.N packRow arr
{-# INLINE_ARRAY packsFormatLn #-}



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
         = AF.toForeignPtr  $ A.convert arrBytes
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
unpacksFormatFixed
        :: (Packable format, A.Target A.A (Value format))
        => format                         -- ^ Fixed length format for each element.
        -> Array Word8                    -- ^ Packed binary data.
        -> Maybe (Array (Value format))   -- ^ Unpacked elements.

unpacksFormatFixed !format !arrBytes
 | lenBytes     <- A.length arrBytes
 , lenElems     <- fieldCount format
 = unsafePerformIO
 $ do   
        let (oStart, _, fptr :: ForeignPtr Word8) 
                = AF.toForeignPtr $ A.convert arrBytes

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
{-# INLINE_ARRAY unpacksFormatFixed #-}


-- | Unpack an array containing elements in some format separated
--   by newline ('\n') characters. 
-- 
--   * Any '\r' characters in the array are filtered out before splitting
--     it into lines.
--   * If the value cannot be converted then this function just calls `error`.
--
unpacksFormatLn
        :: forall format
        .  (Packable format, A.Target A.A (Value format))
        => format                       -- ^ Format for each element.
        -> Array Word8                  -- ^ Packed binary data.
        -> Array (Value format)         -- ^ Unpacked elements.

unpacksFormatLn format arrA8
 = if A.length arrA8 == 0
        then A.empty A.A
        else case eResult of
                Left err        -> error err
                Right (_, arr)  -> arr
 where

        -- Count the number of lines(rows) in the array.
        -- We assume that all rows are terminated by a newline character.
        nLines 
         = A.foldl (\n x -> if x == 0x0a then n + 1 else n)
                   (0 :: Int) arrA8

        (offStart, lenBuffer, fptr :: F.ForeignPtr Word8)
         = AF.toForeignPtr
         $ AG.convert A.F arrA8

        -- Proceed sequentially through the array,
        -- unpacking each element as we go.
        eResult
         =  unsafePerformIO
         $  F.withForeignPtr fptr $ \ptrBufStart
         -> let !ptrStart = F.plusPtr ptrBufStart offStart
                !ptrEnd   = F.plusPtr ptrStart    lenBuffer
            in  flip (A.unfoldEitherOfLengthIO A.A nLines) ptrStart
                $  \ixOut !ptrHere
                -> do   
                        -- Get a pointer to the next newline character,
                        -- or just the character past the end of the buffer
                        -- if there isn't a newline.
                        let findLineEndIx !ptr
                             | ptr >= ptrEnd    
                             = return ptrEnd

                             | otherwise
                             = do !byte  <- F.peek ptr
                                  if  byte == (0x0a :: Word8) 
                                   || byte == (0x0d :: Word8)
                                   then return ptr
                                   else findLineEndIx (F.plusPtr ptr 1)

                        !ptrLine  <- findLineEndIx ptrHere

                        -- The length of the current line.
                        let !lenLine = F.minusPtr ptrLine ptrHere

                        -- Try to unpack a row up to the end of the line.
                        mResult  <- unsafeRunUnpacker (unpack format)
                                        ptrHere lenLine (const False)

                        case mResult of
                         Nothing 
                          -> return 
                          $  Left $ unlines
                                [ "repa-array.unpacksFormatLn: conversion failed"
                                , "  total   lines     = " ++ show nLines
                                , "  current line      = " ++ show ixOut
                                , "  position in chunk = " ++ show (F.minusPtr ptrHere ptrStart)
                                , "  remaining bytes   = " ++ show (F.minusPtr ptrEnd  ptrHere) ]

                         Just (val, ptr') 
                          -> do 
                                -- Skip past new line and line return characters.
                                let skipNewLines !ptr
                                     | ptr >= ptrEnd
                                     = return ptrEnd

                                     | otherwise
                                     = do byte <- F.peek ptr
                                          if  byte == (0x0a :: Word8) 
                                           || byte == (0x0d :: Word8)
                                           then skipNewLines (F.plusPtr ptr 1)
                                           else return ptr

                                ptrSkip <- skipNewLines ptr'
                                return $ Right (ptrSkip, val)
{-# INLINE_ARRAY unpacksFormatLn #-}
