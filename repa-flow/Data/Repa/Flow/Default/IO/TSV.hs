
module Data.Repa.Flow.Default.IO.TSV
        (sourceTSV)
where
import Data.Repa.Flow.Default
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Array                          as A
import Data.Repa.Array.Material                 as A
import Data.Char
import qualified Data.Repa.Flow.Generic.IO      as G
#include "repa-stream.h"


-- | Like `sourceTSV` but take existing file handles.
sourceTSV
        :: Integer              --  Chunk length.
        -> IO ()                --  Action to perform if we find line longer
                                --  than the chunk length.
        -> [Bucket]             --  File paths.
        -> IO (Sources N (Array N (Array F Char)))

sourceTSV nChunk aFail bs
 = do
        -- Rows are separated by new lines, 
        -- fields are separated by tabs.
        let !nl  = fromIntegral $ ord '\n'

--        let !nr  = fromIntegral $ ord '\r'
        -- TODO: what to do about \r?

        let !nt  = fromIntegral $ ord '\t'

        -- Stream chunks of data from the input file, where the chunks end
        -- cleanly at line boundaries. 
        sChunk  <- G.sourceChunks nChunk (== nl) aFail 
                $  A.fromList B bs

        sRows8  <- mapChunks_i (A.diceSep nt nl) sChunk

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
