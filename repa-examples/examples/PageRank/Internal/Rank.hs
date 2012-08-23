{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Internal.Rank 
        (rankInternal)
where
import Internal.Load
import Internal.Step
import External.Titles
import Page
import System.Directory
import Control.Monad
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U


-- | Perform some iterations of the PageRank algorithm.
--   This is an internal version of the algorithm that loads the whole links
--   graph into memory before computing the rank.
rankInternal 
        :: Int                  -- ^ Number of iterations to run.
        -> FilePath             -- ^ Path to links file.
        -> FilePath             -- ^ Path to titles file.
        -> IO ()

rankInternal steps pagesPath titlesPath
 = do   putStrLn "* Loading pages."
        !pages          <- loadPages pagesPath
        let !pageCount  = V.length pages
        let !ranks      = initialRanks pageCount
        pageRank steps pages titlesPath ranks


-- | Construct the initial ranks vector.
initialRanks :: Int -> U.Vector Rank
initialRanks pageCount
 = let  !startRank   = 1 / fromIntegral pageCount
   in   U.replicate pageCount startRank
{-# NOINLINE initialRanks #-}
--  NOINLINE so we can see how much data this function allocates when profiling.


-- | Run several iterations of the internal PageRank algorithm.
pageRank 
        :: Int                  -- ^ Number of iterations to run.
        -> V.Vector Page        -- ^ Pages graph.
        -> FilePath             -- ^ Path to titles file.
        -> U.Vector Rank        -- ^ Initial ranks.
        -> IO ()

pageRank maxIters pages titlesFile ranks0
 = go maxIters ranks0
 where  go 0  _  = return ()
        go !i ranks
         = do   putStr "\n"
                putStrLn $ "* Step " ++ show i
                ranks1  <- stepInternal pages ranks

                -- Sum up the ranks for all the pages, 
                -- this should be very close to 1, minus some some round-off error.
                let rankSum     = U.sum ranks1
                putStrLn $ "  rank sum   : "  ++ show rankSum

                 -- Show the page with the maximum rank.
                let rankMaxIx   = U.maxIndex ranks1
                let rankMax     = ranks1 U.! rankMaxIx
                putStrLn $ "  high ix    : "  ++ show rankMaxIx
                putStrLn $ "  high rank  : "  ++ show rankMax

                -- Write out the pages with the top few ranks.
                putStrLn $ "* Writing top ranks."
                let topRanks    = slurpTopRanks rankMax ranks1

                exists          <- doesDirectoryExist "out"
                when (not exists)
                 $ createDirectory "out"

                let si          = replicate (2 - length (show i)) '0' ++ show i
                mergeRanks ("out/step" ++ si ++ ".ranks") titlesFile topRanks 

                go (i - 1) ranks1
