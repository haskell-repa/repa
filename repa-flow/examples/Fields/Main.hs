
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
import Data.Repa.Array                  as R
import Data.Repa.Array.Foreign          as R
import Data.Repa.Flow                   as R
import Data.Repa.Flows                  as R
import Data.Repa.IO.Flow                as R
import Data.Repa.IO.Flows               as R
import Control.Monad
import Data.Char
import Data.Word
import System.Environment
import Debug.Trace
import Prelude                          as P

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
        sIn        <- fileSourceRecordsF 
                        fileIn (64 * 1024) (== nl)
                        (error "over long line")
 
        -- Dice the chunks of data into arrays of lines and fields.
        sFields    <- map_i (diceOn nt nl) sIn

        -- Do a ragged transpose the chunks, so we get a columnar representation.
        sColumns   <- map_i ragspose3 sFields

        -- Peek at the first chunk to see how many columns we have.
        ([k1], sColumns') <- peek_i 1 sColumns
        let cols   = R.length k1

        -- Concatenate the fields in each column.
        sColumnsC  :: Source (Vector B (Vector F Word8))
                   <- map_i  (R.computeS . R.map (R.intercalate fl)) sColumns'

{-      sColumnsC  <- watch_i sColumnsC'
                   $  \c -> putStrLn 
                          $ show $ P.map (P.map (chr . fromIntegral))
                          $ R.toLists c
-}
        -- Open an output file for each of the columns.
        let filesOut = [fileIn ++ "." ++ show n | n <- [0 .. cols - 1]]
        ooOut   <- fileSinksBytesF filesOut
        putStrLn $ show filesOut

        -- Chunks are distributed into each of the output files.
        oOut    <- distributes_o ooOut (error "spilled field")

        -- Drain all the input chunks into the output files.
        drain sColumnsC oOut

