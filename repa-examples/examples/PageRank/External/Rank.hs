{-# LANGUAGE BangPatterns #-}
module External.Rank
        (rankExternal)
where
import External.Count
import External.Step
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
pageRank :: Int                  -- ^ Iterations to run.
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
                ranks1  <- step pageFile lineCount maxPageId ranks

                -- Get the rank sum, should be 1.
                let rankSum     = U.sum ranks1
                putStrLn $ "  rank sum  : "  ++ show rankSum

                -- Get the maximum rank
                let rankMaxIx   = U.maxIndex ranks1
                let rankMax     = ranks1 U.! rankMaxIx
                putStrLn $ "  high ix   : "  ++ show rankMaxIx
                putStrLn $ "  high rank : "  ++ show rankMax

                -- Write the top ranks
                putStrLn $ "* Writing top ranks."
                let topRanks    = slurpTopRanks rankMax ranks1

                exists          <- doesDirectoryExist "out"
                when (not exists)
                 $ createDirectory "out"

                let si          = P.replicate (2 - P.length (show i)) '0' ++ show i
                mergeRanks ("out/step" ++ si ++ ".ranks") titlesFile topRanks 

                go (i - 1) ranks1


-- | Given the dense PageRank vector, 
--   slurp out the page ids and ranks of the top ranked pages.
slurpTopRanks 
        :: Rank
        -> U.Vector Rank
        -> U.Vector (PageId, Rank)

slurpTopRanks rankMax ranks
        = U.filter  (\(_pid, rank) -> rank >= rankMax * 0.01)
        $ U.zip     (U.enumFromN 0 (U.length ranks))
                    ranks


-- | Given some PageIds and their ranks, 
--   lookup their titles from the titles file, 
--   and pretty print the lot to the result file.
mergeRanks 
        :: FilePath             -- Result file
        -> FilePath             -- Titles file
        -> U.Vector (PageId, Rank)
        -> IO ()

mergeRanks resultPath titlesPath ranks
 = do   hOut   <- openFile resultPath WriteMode

        -- Build a map of the pages we want, and their ranks.
        let mm  = M.fromList
                $ U.toList ranks

        -- Read titles and add to this ref.
        outRef   <- newIORef []
        bsTitles <- BL.readFile titlesPath
        eat outRef mm 1 $ BL.lines bsTitles

        -- Sort the resulting titles.
        outList         <- readIORef outRef
        outVec'         <- V.thaw $ V.fromList outList
        VA.sortBy compareRanks outVec'
        outVec_sorted   <- V.freeze outVec'

        -- Write out to file.
        V.mapM_ (\(pid, rank, title)
                -> hPutStrLn hOut
                        $  padR 10 (show pid) 
                        ++ " "
                        ++ padL 25 (show rank)
                        ++ ": " 
                        ++ BL.unpack title)
            outVec_sorted

        return ()

 where  eat _ _ _ []
         = return ()

        eat !outRef !mm !pid (!title : rest)
         = case M.lookup pid mm of
            Nothing     
             ->    eat outRef mm (pid + 1) rest

            Just rank
             -> do out <- readIORef outRef
                   writeIORef outRef ((pid, rank, title) : out)
                   eat outRef mm (pid + 1) rest

        {-# INLINE compareRanks #-}
        compareRanks (_, r1, _) (_, r2, _)
         = compare r2 r1
