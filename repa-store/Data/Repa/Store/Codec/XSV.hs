
module Data.Repa.Store.Codec.XSV
        ( consumeEscapeXSV
        , splitXSV
        , escapeQuoted )
where
import Data.Repa.Flow.Auto                              as F
import Data.Repa.Flow.Auto.IO                           as F
import Data.Repa.Array.Generic                          as A
import Data.Repa.Array.Material                         as A
import Data.HashMap.Strict                              (HashMap)
import Data.Text                                        (Text)
import qualified Data.Repa.Array.Generic                as AG
import qualified Data.Repa.Flow.Generic                 as G
import qualified Data.Repa.Flow.Chunked                 as C
import qualified Data.HashMap.Strict                    as HM
import Data.Repa.Scalar.Option
import Data.Word
import Data.Char
#include "repa-store.h"


---------------------------------------------------------------------------------------------------
-- | Consume an XSV file with an IO action.
--
--   Along the way we also escape any hard newline or tab characters within
--   double quoted strings. 
--
consumeEscapeXSV
        :: Char         -- ^ Separating character.
        -> FilePath     -- ^ File to consume.
        -> IO ()        -- ^ Action to run if the file is empty.
        -> (HashMap Text Int -> Array B Text -> IO ())
                        -- ^ Function gets a hash of the header fields to their position,
                        --   an array of all the fields for each row.
        -> IO ()

consumeEscapeXSV sepField filePath ffail eat 
 = do   
        ss      <-  splitXSV (fromIntegral $ ord sepField)
                              (1024 * 1024)
                =<< escapeQuoted
                =<< normalizeNewlines
                =<< F.fromFiles [filePath] F.sourceBytes

        hb       <- F.head_i 0 1 ss                
        case hb of
         Just (fsHeader : _, ssBody)
          -> do let (fieldNames :: HashMap Text Int)
                        = HM.fromList
                        $ zip   (AG.toList $ AG.convert B fsHeader)
                                [0..]

                F.consumeS ssBody 
                 $ \_ row -> eat fieldNames (AG.convert B row)

         _ -> ffail
{-# INLINE_FLOW consumeEscapeXSV #-}


---------------------------------------------------------------------------------------------------
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
        sChunk   <-  G.chunkOn_i A.A maxLen (== 0x0a)
                 =<< G.unchunk_i ss

        -- Dice chunks into rows and fields.
        sRows    <-  G.map_i (AG.convert A . diceSep sepField 0x0a) sChunk
        return sRows
{-# NOINLINE splitXSV #-}


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
{-# NOINLINE escapeQuoted #-}


-- | Normalize the various newline conventions to use just a single newline character.
normalizeNewlines :: Sources Word8 -> IO (Sources Word8)
normalizeNewlines ss
 = let  
        -- previous char was LF
        step (Some 0x0a) c
         = case c of
                -- Got LF+CR
                0x0d    -> (None, A.empty A)
                _       -> step None c

        -- previous char was a CR
        step (Some 0x0d) c
         = case c of
                -- Got a CR+LF
                0x0a    -> (None, A.empty A)
                _       -> step None c

        step _ c
         = case c of
                0x0a    -> (Some 0x0a, A.singleton A 0x0a)
                0x0d    -> (Some 0x0d, A.singleton A 0x0a)
                _       -> (None,      A.singleton A c)

   in   C.process_i step (None :: Option Word8) ss
{-# NOINLINE normalizeNewlines #-}


