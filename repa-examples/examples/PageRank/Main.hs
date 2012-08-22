{-# LANGUAGE BangPatterns #-}

import External.Count
import External.Rank
import System.Environment


main :: IO ()
main
 = do   args    <- getArgs
        case args of
         -- Just count the number of pages in the pages file.
         ["-count", pagesPath] 
          -> do _       <- countPages pagesPath
                return ()

         ["-rank-external", pagesPath, titlesPath] 
                -> rankExternal pagesPath titlesPath

         _      -> error "bad usage"
        


