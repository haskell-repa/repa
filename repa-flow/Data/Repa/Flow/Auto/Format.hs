

module Data.Repa.Flow.Auto.Format
        ( -- * Pre-defined data formats
          module Data.Repa.Convert.Formats

          -- * Packing functions
        , packFormat_i
        , concatPackFormat_i
        , unlinesPackFormat_i)
where
import Data.Word
import Data.Char
import Data.Repa.Flow.Auto                      as F
import Data.Repa.Array                          as A
import Data.Repa.Array.Auto.Format              as A
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Convert.Format       as C
import Data.Repa.Convert.Formats
#include "repa-flow.h"


-- | Pack elements into the given storage formats.
packFormat_i
        :: (C.Packable format, Elem (Value format), Build (Array Word8) t)
        => format                       -- ^ Desination format for data.
        -> Sources (Value format)       -- ^ Sources of values to be packed.
        -> IO (Sources (Array Word8))   -- ^ Packed data.

packFormat_i format ss
 = let
        packElem x 
         = let Just arr = A.packFormat format x
           in  arr
        {-# INLINE packElem #-}

   in   G.map_i (A.map packElem) ss
{-# INLINE_FLOW packFormat_i #-}


-- | Like `packFormat_i`, 
--   but append the packed output arrays into a flat stream of bytes.
concatPackFormat_i
        :: (C.Packable format, Elem (Value format), Build (Array Word8) t)
        => format                       -- ^ Destination format for data.
        -> Sources (Value format)       -- ^ Sources of values to be packed.
        -> IO (Sources Word8)           -- ^ Packed data.

concatPackFormat_i format ss 
        =   G.map_i A.concat 
        =<< packFormat_i format ss
{-# INLINE_FLOW concatPackFormat_i #-}


-- | Like `concatPackFormat_i`, 
--   but also insert a newline character after each array.
unlinesPackFormat_i
        :: (C.Packable format, Elem (Value format), Build (Array Word8) t)
        => format                       -- ^ Destination format for data.
        -> Sources (Value format)       -- ^ Sources of values to be packed.
        -> IO (Sources Word8)           -- ^ Packed data.

unlinesPackFormat_i format ss
        =   G.map_i A.concat
        =<< F.map_i (\arr -> A.concat $ A.fromList [arr, nl])
        =<< packFormat_i format ss
        where   !nl = A.fromList [fromIntegral $ ord '\n']
{-# INLINE_FLOW unlinesPackFormat_i #-}

