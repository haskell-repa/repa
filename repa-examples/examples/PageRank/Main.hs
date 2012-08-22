{-# LANGUAGE BangPatterns #-}

import Count
import Rank
import System.IO
import System.Environment


main :: IO ()
main
 = do   args    <- getArgs
        case args of
         -- Just count the number of pages in the pages file.
         ["-count", pagesPath] 
          -> do (lines, pages)  <- countPages pagesPath
                return ()

         ["-rank-incremental", pagesPath, titlesPath] 
                -> runRankIncremental pagesPath titlesPath

         _      -> error "bad usage"
        


