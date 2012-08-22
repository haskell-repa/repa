{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Internal.Rank where
import Page
import Progress
import qualified Data.ByteString.Lazy.Char8     as BL
import qualified Data.Vector                    as V


rankInternal :: FilePath -> FilePath -> IO ()
rankInternal pagesPath _titlesPath
 = do   _       <- loadPages pagesPath
        return ()


-- | Add rank contributions due to forward-links to a ranks vector.
loadPages
        :: FilePath             -- ^ Pages file.
        -> IO (V.Vector Page)

loadPages filePath 
 = do   bs      <- BL.readFile filePath
        _       <- go 0 (1 :: Int) (BL.lines bs)
        return undefined

 where  go ixLine _ []
         = do   printPosition True "  lines read: " 10000 ixLine
                return lines

        go !ixLine !ixPage (l : ls)
         = do   -- Print how far along we are.
                printPosition False "  lines read: " 10000 ixLine

                -- Parse the page
                let !_page      = parsePage l

                go (ixLine + 1) ixPage ls



