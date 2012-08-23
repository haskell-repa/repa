{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Internal.Rank where
import Internal.Load

rankInternal :: FilePath -> FilePath -> IO ()
rankInternal pagesPath _titlesPath
 = do   _       <- loadPages pagesPath
        return ()


