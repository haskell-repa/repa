{-# LANGUAGE BangPatterns #-}
module External.Rank
        (rankExternal)
where
import External.Count
import External.Step
import External.Titles
import Page
import Progress
import System.IO
import System.Environment
import Data.IORef
import System.Directory
import Control.Monad
import Prelude                                  as P
import Data.Text                                as T
import qualified Data.Vector.Algorithms.Heap    as VA
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector                    as V
import qualified Data.IntMap                    as M
import qualified Data.ByteString.Lazy.Char8     as BL

-- TODO: Add in alpha parameter so we can compare against baseline.
--       Show difference between previous and current vector, to check convergence.
--
rankExternal :: FilePath -> FilePath -> IO ()
rankExternal pagesPath titlesPath
 = do   (lineCount, maxPageId)  <- countPages pagesPath
        let pageCount   = maxPageId + 1

        let startRank   = 1 / fromIntegral pageCount
        let ranks       = U.replicate pageCount startRank

        pageRank 10 pagesPath titlesPath lineCount pageCount ranks


-- | Run several iterations of the algorithm.
pageRank :: Int                 -- ^ Iterations to run.
        -> FilePath             -- ^ Pages file.
        -> FilePath             -- ^ Titles file.
        -> Int                  -- ^ Total number of lines in the file.
        -> Int                  -- ^ Maximum Page Id
        -> U.Vector Rank        -- ^ Initial pageranks.     
        -> IO ()

pageRank maxIters pageFile titlesFile lineCount maxPageId ranks0
 = go maxIters ranks0
 where  go 0 _ = return ()
        go !i ranks
         = do   putStr   "\n"
                putStrLn $ "* Step " ++ show i
                ranks1   <- stepExternal pageFile lineCount maxPageId ranks

                -- Sum up the ranks for all the pages, 
                -- this should be very close to 1, minus some some round-off error.
                let rankSum     = U.sum ranks1
                putStrLn $ "  rank sum  : "  ++ show rankSum

                -- Show the page with the maximum rank.
                let rankMaxIx   = U.maxIndex ranks1
                let rankMax     = ranks1 U.! rankMaxIx
                putStrLn $ "  high ix   : "  ++ show rankMaxIx
                putStrLn $ "  high rank : "  ++ show rankMax

                -- Write out the pages with the top few ranks.
                putStrLn $ "* Writing top ranks."
                let topRanks    = slurpTopRanks rankMax ranks1

                exists          <- doesDirectoryExist "out"
                when (not exists)
                 $ createDirectory "out"

                let si          = P.replicate (2 - P.length (show i)) '0' ++ show i
                mergeRanks ("out/step" ++ si ++ ".ranks") titlesFile topRanks 

                go (i - 1) ranks1


-- | Given the dense PageRank vector, 
--   slurp out the page ids and ranks for some of the
--   highest ranked pages.
slurpTopRanks 
        :: Rank
        -> U.Vector Rank
        -> U.Vector (PageId, Rank)

slurpTopRanks rankMax ranks
        = U.filter  (\(_pid, rank) -> rank >= rankMax * 0.01)
        $ U.zip     (U.enumFromN 0 (U.length ranks))
                    ranks


