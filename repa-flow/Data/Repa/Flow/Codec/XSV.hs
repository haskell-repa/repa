
module Data.Repa.Flow.Codec.XSV
        ( splitXSV
        , escapeQuoted )
where
import Data.Repa.Flow.Auto.Base
import Data.Repa.Array.Generic                          as A
import Data.Repa.Array.Material.Auto                    as A
import Data.Repa.Array.Material                         as A
import qualified Data.Repa.Array.Generic.Convert        as G
import qualified Data.Repa.Flow.Generic                 as G
import qualified Data.Repa.Flow.Chunked                 as C
import Data.Word
import Data.Char
#include "repa-flow.h"


-- | Split an XSV file into rows of fields.
--
--   Fields are terminated by the provided separator, 
--   and rows are terminated by newline characters.
--
--   TODO: call a fail action if we find an over-long line.
--
splitXSV
        :: Word8                -- ^ Field separator byte.
        -> Int                  -- ^ Maximum allowable line length, in characters.
        -> Sources Word8        -- ^ Sources of bytes
        -> IO (Sources (Array A (Array A Word8)))    
                                -- ^ Arrays of rows of fields.
splitXSV sepField maxLen ss
 = do   
        -- TODO: Check for over-long chunks and call aFail

        -- Escape the source data and chunk it on line boundaries.
        sChunk   <-  G.chunkOn_i A.A maxLen (== 0x0a)                   -- end on newline.
                 =<< G.unchunk_i ss

        -- Dice chunks into rows and fields.
        sRows    <-  G.map_i (G.convert    . A.diceSep sepField 0x0a    -- dice on newline.
                                           . A.filter A (/= 0x0d))      -- drop carriage returns.
                             sChunk
        return sRows
{-# INLINE_FLOW splitXSV #-}


-- | Escape hard newlines and tab characters within quoted strings.
escapeQuoted :: Sources Word8 -> IO (Sources Word8)
escapeQuoted ss
 = let  
        anl     = A.fromList A $ map (fromIntegral . ord) "\\n"
        atab    = A.fromList A $ map (fromIntegral . ord) "\\t"
   
        step s@False c
         = case c of
                0x22    -> (True,  A.singleton A c)     -- double quote
                _       -> (s,     A.singleton A c)

        step s@True c  
         = case c of
                0x0a    -> (s,     anl)                 -- newline
                0x09    -> (s,     atab)                -- tab
                0x22    -> (False, A.singleton A c)     -- double quote
                _       -> (s,     A.singleton A c)

   in   C.process_i step False ss
{-# INLINE_FLOW escapeQuoted #-}
