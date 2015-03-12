
module Data.Repa.Flow.Generic.IO.XSV
        ( sourceCSV
        , sourceTSV)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Flow.Generic.Map
import Data.Repa.Flow.Generic.IO.Base
import Data.Repa.Array                          as A
import Data.Repa.Array.Material                 as A
import Data.Char
#include "repa-flow.h"


-------------------------------------------------------------------------------
-- | Read a file containing Comma-Separated-Values.
--
--   TODO: handle escaped commas.
--   TODO: check CSV file standard.
--
sourceCSV
        :: BulkI l Bucket
        => Integer              --  Chunk length.
        -> IO ()                --  Action to perform if we find line longer
                                --  than the chunk length.
        -> Array l Bucket       --  File paths.
        -> IO (Sources Int IO (Array N (Array N (Array F Char))))

sourceCSV nChunk aFail bs
 = do
        -- Rows are separated by new lines, 
        -- fields are separated by commas.
        let !nl  = fromIntegral $ ord '\n'
        let !nr  = fromIntegral $ ord '\r'
        let !nt  = fromIntegral $ ord ','

        -- Stream chunks of data from the input file, where the chunks end
        -- cleanly at line boundaries. 
        sChunk  <- sourceChunks nChunk (== nl) aFail bs
        sRows8  <- map_i (A.diceSep nt nl . A.filter U (/= nr)) sChunk

        -- Convert element data from Word8 to Char.
        -- Chars take 4 bytes each, but are standard Haskell and pretty
        -- print properly. We've done the dicing on the smaller Word8
        -- version, and now map across the elements vector in the array
        -- to do the conversion.
        sRows   <- map_i (A.mapElems (A.mapElems 
                            (A.computeS F . A.map (chr . fromIntegral))))
                         sRows8

        return sRows
{-# INLINE sourceCSV #-}


-------------------------------------------------------------------------------
-- | Read a file containing Tab-Separated-Values.
sourceTSV
        :: BulkI l Bucket
        => Integer              --  Chunk length.
        -> IO ()                --  Action to perform if we find line longer
                                --  than the chunk length.
        -> Array l Bucket       --  File paths.
        -> IO (Sources Int IO (Array N (Array N (Array F Char))))

sourceTSV nChunk aFail bs
 = do
        -- Rows are separated by new lines, 
        -- fields are separated by tabs.
        let !nl  = fromIntegral $ ord '\n'
        let !nr  = fromIntegral $ ord '\r'
        let !nt  = fromIntegral $ ord '\t'

        -- Stream chunks of data from the input file, where the chunks end
        -- cleanly at line boundaries. 
        sChunk  <- sourceChunks nChunk (== nl) aFail bs
        sRows8  <- map_i (A.diceSep nt nl . A.filter U (/= nr)) sChunk

        -- Convert element data from Word8 to Char.
        -- Chars take 4 bytes each, but are standard Haskell and pretty
        -- print properly. We've done the dicing on the smaller Word8
        -- version, and now map across the elements vector in the array
        -- to do the conversion.
        sRows   <- map_i (A.mapElems (A.mapElems 
                            (A.computeS F . A.map (chr . fromIntegral))))
                         sRows8

        return sRows
{-# INLINE sourceTSV #-}

