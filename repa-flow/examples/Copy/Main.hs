{-# LANGUAGE BangPatterns #-}
import Data.Array.Repa.Bulk
import Data.Array.Repa.Flow
import Data.Array.Repa.Flow.IO.File
import Control.Monad
import Data.Char
import System.Environment

main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [fileIn, fileOut]      
           -> pcopy  fileIn fileOut

         _ -> putStrLn $ unlines
                [ "Usage: flow-copy <source_file> <result_file>" ]


-- | Copy a file from one place to another, one line at a time.
pcopy :: FilePath -> FilePath -> IO ()
pcopy fileIn fileOut
 = do   
        -- Source whole records from input file.
        let !nl = fromIntegral $ ord '\n'
        ifile   <- fileSourceRecordsF fileIn
                        (1024*1024)
                        (== nl) 
                        (error "fark")

        -- Sink to the output file.
        ofile   <- fileSinkBytesF fileOut

        -- Drain the source into the sink.
        drain ifile ofile
