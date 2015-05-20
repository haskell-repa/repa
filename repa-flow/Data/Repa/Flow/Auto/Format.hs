
module Data.Repa.Flow.Auto.Format
        ( -- * Pre-defined data formats
          module Data.Repa.Convert.Formats

          -- * Packing functions
        , packFormat_i
        , packFormatLn_i
        , packAsciiLn_i
        , keyPackAsciiLn_i)
where
import Data.Word
import Data.Char
import Data.Repa.Convert.Formats
import Data.Repa.Flow.Auto                              as F
import Data.Repa.Array                                  as A
import Data.Repa.Array.Auto.Format                      as A
import qualified Data.Repa.Convert                      as C
import qualified Data.Repa.Flow.Generic                 as G
#include "repa-flow.h"


-- | Pack elements into the given storage formats.
packFormat_i
        :: (C.Packable format, Elem (Value format))
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


-- | Like `packFormat_i`, but also append a newline character
--   after every packed element.
packFormatLn_i
        :: (C.Packable format, Elem (Value format))
        => format                       -- ^ Destination format for data.
        -> Sources (Value format)       -- ^ Sources of values to be packed.
        -> IO (Sources (Array Word8))   -- ^ Packed data.

packFormatLn_i format ss
        =   F.map_i (\arr -> A.concat $ A.fromList [arr, nl])   -- TODO: avoid copy
        =<< packFormat_i format ss
        where   !nl = A.fromList [fromIntegral $ ord '\n']
{-# INLINE_FLOW packFormatLn_i #-}


-- | Like `packFormatLn_i`,
--   but use a default, human-readable format to encode the values.
packAsciiLn_i
        :: forall a 
        . ( C.FormatAscii a, a ~ Value (C.FormatAscii' a)
          , Elem a
          , Packable (C.FormatAscii' a))
        => Sources a                    -- ^ Sources of values to be packed.
        -> IO (Sources (Array Word8))   -- ^ Packed data.

packAsciiLn_i ss
        =   F.map_i (\arr -> A.concat $ A.fromList [arr, nl])    -- TODO: avoid copy
        =<< packFormat_i (C.formatAscii proxy) ss
        where   !nl     = A.fromList [fromIntegral $ ord '\n']
                proxy   = (error "repa-flow: packAscii_i proxy" :: a)
{-# INLINE_FLOW packAsciiLn_i #-}



---------------------------------------------------------------------------------------------------
-- | Like `packFormatLn_i`,
--   but use a default, human-readable format to encode the values.
keyPackAsciiLn_i
        :: forall a k t
        . ( C.FormatAscii a, a ~ Value (C.FormatAscii' a)
          , Elem a
          , Packable (C.FormatAscii' a)
          , Elem k, Build k t)
        => Sources (k, a)                       -- ^ Sources of values to be packed.
        -> IO (Sources (k, Array Word8))        -- ^ Packed data.

keyPackAsciiLn_i ss
 = let  
        proxy   = (error "repa-flow: sndPackAsciiLn_i proxy" :: a)
        !nl     = A.fromList [fromIntegral $ ord '\n']
        packElem (k, x)
         = let  Just arr = A.packFormat (C.formatAscii proxy) x
           in   (k, A.concat $ A.fromList [arr, nl])            -- TODO: avoid copy
        {-# INLINE packElem #-}

   in   G.map_i (A.map packElem) ss
{-# INLINE_FLOW keyPackAsciiLn_i #-}


