
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
#include "repa-flow.h"


-- | Split an XSV file into rows of fields.
--
--   Fields are terminated by the provided separator, 
--   and rows are terminated by newline characters.
--
--   TODO: call a fail action if we find an over-long line.
--
splitXSV
        :: Char                 -- ^ Field separator, eg '\n' or '\t'.
        -> Int                  -- ^ Maximum allowable line length, in characters.
        -> Sources Char         -- ^ Sources of characters.
        -> IO (Sources (Array A (Array A Char)))    
                                -- ^ Arrays of rows of fields.
splitXSV sepField maxLen ss
 = do   
        -- TODO: Check for over-long chunks and call aFail

        -- Escape the source data and chunk it on line boundaries.
        sChunk   <-  G.chunkOn_i A.A maxLen (== '\n') 
                 =<< G.unchunk_i ss

        -- Dice chunks into rows and fields.
        sRows    <-  G.map_i (G.convert    . A.diceSep sepField '\n' 
                                           . A.filter A (/= '\r')) 
                             sChunk
        return sRows


-- | Escape hard newlines and tab characters within quoted strings.
escapeQuoted :: Sources Char -> IO (Sources Char)
escapeQuoted ss
 = let  
        anl     = A.fromList A "\\n"
        atab    = A.fromList A "\\t"
   
        step s@False c
         = case c of
                '\"'    -> (True,  A.singleton A c)
                _       -> (s,     A.singleton A c)

        step s@True c  
         = case c of
                '\n'    -> (s,     anl)
                '\t'    -> (s,     atab)
                '\"'    -> (False, A.singleton A c)
                _       -> (s,     A.singleton A c)

   in   C.process_i step False ss
{-# INLINE_FLOW escapeQuoted #-}
