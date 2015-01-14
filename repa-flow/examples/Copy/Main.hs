{-# LANGUAGE BangPatterns #-}
import Data.Repa.Flow
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
        -- Source from the input file.
        ifile   <- fileSourcesBytes [fileIn] (1024*1024)

        -- Sink to the output file.
        ofile   <- fileSinksBytes   [fileOut]

        -- Drain the source into the sink.
        drain ifile ofile
