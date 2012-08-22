{-# LANGUAGE BangPatterns #-}

import External.Count
import External.Rank
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

         ["-rank-external", pagesPath, titlesPath] 
                -> rankExternal pagesPath titlesPath

         _      -> error "bad usage"
        


