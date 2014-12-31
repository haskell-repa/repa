
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
import Data.Repa.Flow.Chunked
import Data.Repa.Flow.Chunked.IO        
import qualified Data.Repa.Flow.Simple          as S
import qualified Data.Repa.Flow.Generic         as G
import Data.Repa.Array                          as R
import Data.Repa.Array.Foreign                  as R
import Prelude                                  as P
import System.Environment
import Data.Char
import Data.Word


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [fileIn] -> pfields fileIn
         _ -> putStrLn $ unlines
                [ "Usage: flow-fields <source_file>"]


pfields :: FilePath -> IO ()
pfields fileIn
 = do   
        -- Rows are separated by new lines, 
        -- fields are separated by tabs.
        let !nl  = fromIntegral $ ord '\n'
        let !nt  = fromIntegral $ ord '\t'
        let !(fl  :: Vector F Word8) = R.vfromList [nl]

        -- Stream chunks of data from the input file,
        -- where the chunks end cleanly at record boundaries.
        sIn     ::  Sources () IO F Word8
                <-  G.project_i (zero 1)
                =<< fileSourcesRecords 
                        [fileIn] 256 (== nl)
                        (error "over long line")
 
        -- Dice the chunks of data into arrays of lines and fields.
        sFields'   <- mapChunks_i (diceOn nt nl) sIn

        -- Do a ragged transpose the chunks, so we get a columnar representation.
        sColumns   <- mapChunks_i ragspose3 sFields

        -- Concatenate the fields in each column.
        sColumnsC  :: Sources () IO B (Vector F Word8)
                   <- mapChunks_i (R.computeS . R.map (R.intercalate fl)) sColumns

        -- Peek at the first chunk to see how many columns we have.
        ([k1], sColumnsC) <- S.peek_i 1 sColumnsC
        let cols   = R.length k1

        -- Open an output file for each of the columns.
        let filesOut = [fileIn ++ "." ++ show n | n <- [0 .. cols - 1]]
        ooOut      <- fileSinksBytes filesOut

        -- Chunks are distributed into each of the output files.
        oOut       <- G.distributes_o ooOut (error "spilled field")

        -- Drain all the input chunks into the output files.
        drain sColumnsC oOut

