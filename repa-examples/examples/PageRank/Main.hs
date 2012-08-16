{-# LANGUAGE BangPatterns #-}

import Page
import Step
import Progress
import Data.Conduit.Binary                      as B
import Data.Conduit.List                        as C
import Data.Conduit.Text                        as T
import Data.Conduit                             as C
import qualified Data.Vector.Algorithms.Heap    as VA
import qualified Data.Vector.Unboxed            as U
import Prelude                                  as P
import System.IO
import System.Environment

-- TODO: Scores for dangling pages aren't being added
--       Because the links file doesn't contain records for pages with no out-links
--       Should count these in getMaxPageId pass.
--       Also determine maximum out-links at this stage, 
--          inc compute average out-links.
--       Add in alpha parameter so we can compare against baseline.
--       Show difference between previous and current vector, to check convergence.
--
main :: IO ()
main
 = do   [pagesPath]             <- getArgs

        (lineCount, pageCount)  <- countPages pagesPath

        let startRank   = 1 / fromIntegral pageCount
        let ranks       = U.replicate pageCount startRank

        run 10 pagesPath lineCount pageCount ranks


-- | Run several iterations of the algorithm.
run     :: Int                  -- ^ Iterations to run.
        -> FilePath             -- ^ Pages file.
        -> Int                  -- ^ Total number of lines in the file.
        -> Int                  -- ^ Maximum Page Id
        -> U.Vector Rank        -- ^ Initial pageranks.     
        -> IO ()

run maxIters pageFile lineCount maxPageId ranks0
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
                topRanks        <- slurpTopRanks rankMax ranks1
                let si  = replicate (2 - length (show i)) '0' ++ show i
                writeRanks ("out/step" ++ si ++ ".ranks") topRanks 

                go (i - 1) ranks1


-- | Get the maximum page number in this file.
countPages :: FilePath -> IO (Int, Int)
countPages !filePath
 = do   putStrLn $ "* Counting pages in file."

        (maxPageId, lineCount)
                <- C.runResourceT
                $  B.sourceFile filePath
                $= B.lines
                $= T.decode T.utf8
                $$ C.foldM eat (0, 0)

        let pageCount   = maxPageId + 1

        printPosition True "  lines read: " 10000 lineCount
        putStrLn $ " max page id: " ++ padR 10 (show maxPageId)
        return (lineCount, pageCount)

 where  eat (!maxPageId, !lineCount) !line
         = unsafeLiftIO
         $ do   let Just pid    = parsePageId line
                printPosition False "  lines read: " 10000 lineCount
                let !maxPageId' = max pid maxPageId
                return (maxPageId', lineCount + 1)
        {-# INLINE eat #-}

slurpTopRanks 
        :: Rank
        -> U.Vector Rank
        -> IO (U.Vector (PageId, Rank))

slurpTopRanks rankMax ranks
 = do   pidRanks 
                <- U.thaw
                $ U.filter  (\(_pid, rank) -> rank >= rankMax * 0.01)
                $ U.zip     (U.enumFromN 0 (U.length ranks))
                            ranks

        VA.sortBy compare pidRanks
        U.freeze pidRanks


writeRanks 
        :: FilePath 
        -> U.Vector (PageId, Rank)
        -> IO ()

writeRanks filePath ranks
 = do   hFile   <- openFile filePath WriteMode
        go hFile 0
        hClose hFile

 where  go h !i
         | i >= U.length ranks
         = return ()

         | otherwise
         = do   let (pid, rank) = ranks U.! i
                hPutStr  h (padR 10 (show pid) ++ ": " ++ (show $ rank))
                hPutChar h '\n'
                go h (i + 1)

