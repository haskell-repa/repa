
module Data.Repa.Flow.IO.TSV
        (sourceTSV)
where
import Data.Repa.Flow
import Data.Repa.Array                          as A
import Data.Repa.Array.Material                 as A
import qualified Data.Repa.Flow.Generic         as G hiding (next)
import System.IO
import Data.Char
#include "repa-stream.h"


-- | Like `sourceTSV` but take existing file handles.
sourceTSV
        :: Int                  --  Chunk length.
        -> IO ()                --  Action to perform if we find line longer
                                --  than the chunk length.
        -> [Handle]             --  File paths.
        -> IO (Sources N (Array N (Array F Char)))

sourceTSV nChunk aFail hs
 = do
        -- Rows are separated by new lines, 
        -- fields are separated by tabs.
        let !nl  = fromIntegral $ ord '\n'
        let !nr  = fromIntegral $ ord '\r'
        let !nt  = fromIntegral $ ord '\t'

        -- Stream chunks of data from the input file, where the chunks end
        -- cleanly at line boundaries. 
        sChunk  <- G.sourceChunks nChunk (== nl) aFail hs

        -- Dice the chunks of data into arrays of lines and fields.
        let isWhite c = c == nl || c == nr || c == nt
            {-# INLINE isWhite #-}

        sRows8  <- mapChunks_i 
                     (A.mapElems (A.trimEnds isWhite) . A.diceOn nt nl) 
                     sChunk

        -- Convert element data from Word8 to Char.
        -- Chars take 4 bytes each, but are standard Haskell and pretty
        -- print properly. We've done the dicing on the smaller Word8
        -- version, and now map across the elements vector in the array
        -- to do the conversion.
        sRows   <- mapChunks_i 
                     (A.mapElems (A.mapElems 
                        (A.computeS F . A.map (chr . fromIntegral))))
                     sRows8

        return sRows
{-# INLINE sourceTSV #-}
