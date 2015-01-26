{-# LANGUAGE BangPatterns #-}
import Data.Repa.Flow
import Data.Repa.Flow.IO.Default
import System.Environment
import Control.Monad
import Data.Char

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
        -- Source from the input file.
        ifile   <- fromFiles [fileIn]  sourceBytes

        -- Sink to the output file.
        ofile   <- toFiles   [fileOut] sinkBytes

        -- Drain the source into the sink.
        drain ifile ofile
