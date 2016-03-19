
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
        , UnpackableRow(..)
        , A.Unpackables)
where
import Data.Repa.Array.Auto.Base                                        as A
import Data.Repa.Array.Generic.Convert                                  as A
import qualified Data.Repa.Array.Generic                                as AG
import qualified Data.Repa.Array.Material.Auto                          as A
import qualified Data.Repa.Array.Material.Auto.Operator.Lines           as A
import qualified Data.Repa.Array.Material.Auto.Operator.Unpackables     as A
import qualified Data.Repa.Array.Material.Foreign                       as AF
import qualified Data.Repa.Array.Internals.Target                       as A
import qualified Data.Repa.Array.Internals.Layout                       as A
import qualified Data.Repa.Array.Internals.Bulk                         as A
import qualified Data.Repa.Convert.Format                               as C
import Data.Repa.Convert.Format
import Data.Repa.Convert.Formats

import Foreign.ForeignPtr                                               as F
import Foreign.Ptr                                                      as F

import System.IO.Unsafe
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
        :: C.Unpackable format
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



-- | Unpack an encoded table of values from an array of bytes.
unpacksFormatLn
        :: UnpackableRow format
        => Sep format                   -- ^ Format of each row.
        -> Array Word8                  -- ^ Source data.
        -> Array (Value (Sep format))   -- ^ Array of rows.

unpacksFormatLn format arrSrc@(A.AArray_Word8 arrSrcF)
  = unsafePerformIO
  $ do  
        -- Rows are separated by newline characters.
        let !wSepRow    
                = fromIntegral $ ord '\n'

        -- Determine the field separator character.
        -- If the format is just () then we're only counting the 
        -- number of lines in the table, so it doesn't matter
        -- what we use for the field separator.
        let !wSepField  
                = case takeSepChar format of 
                        Nothing -> fromIntegral $ ord ' '
                        Just c  -> fromIntegral $ ord c

        -- Scan through the input data to determine where the lines begin
        -- and end.
        let (ixsStarts, ixsEnds) 
                =  A.findRowsStartEnd wSepRow arrSrc

        -- Allocate a buffer for the output data.
        let len =  A.extent $ A.bufferLayout ixsStarts
        buf     <- A.unsafeNewBuffer (A.Auto len)

        -- Unpack all the rows into the buffer.
        mErr    <- A.unpacksToBuffer 
                        format 
                        wSepField 
                        arrSrcF 
                        ixsStarts ixsEnds buf

        case mErr of
         Nothing        
          ->  A.unsafeFreezeBuffer    buf

         -- TODO: return error.
         Just err
          -> error $ "unsafeFormatLn: parse error " ++ show err
{-# INLINE unpacksFormatLn #-}


-- | Dictionaries needed to unpack a row of the given format.
type UnpackableRow format
        = ( SepFormat format
          , A.Unpackables (Sep format)
          , A.Target A.A  (Value (Sep format)))

