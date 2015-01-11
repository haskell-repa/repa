{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
-- | Sum up segments of doubles according to a segment descriptor file.
--   The number of values written to the output is equal to the number
--   of segments defined by the segments file.
--
-- @ 
--   fileInSegs  fileInVals  fileOutSums
--   ----------  ----------  -----------
--   0           1           
--   0           2           3
--   1           3 
--   1           4           7
--   0           5
--   0           6
--   0           7           18
--   1           8           8
--
-- @
--
module Main where
import Convert
import Data.Repa.Flow.Chunked
import Data.Repa.Flow.Chunked.IO
import Data.Repa.Array                  as A
import Data.Repa.Array.Foreign          as A
import Data.Repa.Array.Unsafe.Nested    as A
import Data.Repa.Array.Unsafe.Unboxed   as A
import Data.Char
import Data.Word
import System.Environment
import qualified Data.Repa.Flow.Generic as G
import Prelude                          as P

main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [fileInSeg, fileInVals, fileOutSums]      
           -> pGroupSum fileInSeg fileInVals fileOutSums
         _ -> putStrLn $ unlines
                [ "Usage: flow-groupsum <source_segs> <source_vals> <result_vals>" ]


-- | Sum up segments of doubles according to a segment descriptor file.
pGroupSum :: FilePath -> FilePath -> FilePath -> IO ()
pGroupSum fileInSegs fileInVals fileOutSums
 = do   
        let !nl  = fromIntegral $ ord '\n'

        -- Group the input segment file to get segment lengths.
        iSegCols   <-  G.project_i (zero 1)
                   =<< fileSourcesRecords [fileInSegs] (64 * 1024) (== nl) 
                                (error "over long line in segs file")

        iSegElems  <- mapChunks_i (A.segmentOn nl) iSegCols

--        iSegGroups :: Sources () IO B (Vector F Word8, Int)
--                   <- groupsBy_i eqVectorF iSegElems

        iSegLens   :: Sources () IO U Int
                   <- map_i snd =<< groupsBy_i eqVectorF iSegElems

        i0        <-  watch_i (\_ arr -> putStrLn 
                                         $ show $ A.toList arr)
                             iSegLens

        oDiscard  <-  discard_o ()
        drain i0 oDiscard

{-
        -- Read floating point values from input file.
        let !nl  = fromIntegral $ ord '\n'
        let !nr  = fromIntegral $ ord '\r'
        let !nt  = fromIntegral $ ord '\t'
        let isWhite c = c == nl || c == nr || c == nt
            {-# INLINE isWhite #-}

        istrings <-  mapChunks_i (R.trimEnds isWhite)
                 =<< G.project_i (zero 1)
                 =<< fileSourcesRecords [fileInVals] (64 * 1024) (== nl)
                        (error "over long line in vals file")

        ivals    <- map_i readDouble istrings


        -- Sum up values according to the segment descriptor.
        isums    <- folds_ii (+) 0 isegLens ivals

        -- Create a sink that converts floats back to strings and writes
        -- them to the output file.
        ofile    <-  G.project_i (zero 1)
                 =<< fileSinksBytes fileOutSums

        ofloat   <- map_o (showDoubleFixed 2) ofile

        -- Drain sums into the output.
        drain isums ofloat

-}
