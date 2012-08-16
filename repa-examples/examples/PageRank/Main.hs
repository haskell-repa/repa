{-# LANGUAGE BangPatterns #-}


import Page
import Step

import Data.Conduit.Binary                      as B
import Data.Conduit.List                        as C
import Data.Conduit.Text                        as T
import Data.Conduit                             as C
import System.Environment
import Control.Monad
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import qualified Data.ByteString.Char8          as BC
import qualified Data.Map                       as Map
import Data.Map                                 (Map)
import Data.Vector.Unboxed.Mutable              (IOVector)
import Prelude                                  as P
import Data.List
import System.IO


-- TODO: Scores for dangling pages aren't being added
--       Because the links file doesn't contain records for pages with no out-links
--       Should count these in getMaxPageId pass.
--       Also determine maximum out-links at this stage, 
--          inc compute average out-links.
--       Add in alpha parameter so we can compare against baseline.
--       Show top-10 pageranks.
--       Show difference between previous and current vector, to check convergence.
--
main
 = do   [pagesPath]      <- getArgs

        putStrLn $ "* Get maximum page id."
        pageCount       <- liftM (+ 1) $ getMaxPageId pagesPath
        putStrLn $ "  pages = " ++ show pageCount

        let startRank   = 1 / fromIntegral pageCount
        let ranks       = U.replicate pageCount startRank

        putStrLn $ "* Computing page ranks."
        run 10 pagesPath pageCount ranks


-- | Run several iterations of the algorithm.
run     :: Int                  -- ^ Iterations to run.
        -> FilePath             -- ^ Pages file.
        -> Int                  -- ^ Total number of pages in the file.
        -> U.Vector Rank        -- ^ Initial pageranks.     
        -> IO ()

run i pageFile pageCount ranks0
 = go i ranks0
 where  go 0 _ = return ()
        go !i ranks
         = do   putStrLn $ "* Step " ++ show i
                ranks1  <- step pageFile pageCount ranks

                -- Write the current ranks out to file.
                putStrLn $ "* Writing ranks."
                let si  = replicate (2 - length (show i)) '0' ++ show i
                writeRanks ("out/step" ++ si ++ ".ranks") ranks1 

                -- Get the rank sum, should be 1.
                let rankSum     = U.sum ranks1
                putStrLn $ "* Rank Sum = "  ++ show rankSum

                -- Get the maximum rank
                let rankMax     = U.maxIndex ranks1
                putStrLn $ "* Rank Index = " ++ show rankMax

                go (i - 1) ranks1


-- | Get the maximum page number in this file.
getMaxPageId :: FilePath -> IO Int
getMaxPageId !filePath
 =  C.runResourceT
 $  B.sourceFile filePath
 $= B.lines
 $= T.decode T.utf8
 $$ C.foldM eat 0

 where  eat !n !line
         = do   let Just pid    = parsePageId line

                when (n `mod` 100000 == 0)
                 $ unsafeLiftIO $ print n

                let !n' = max pid n
                return n'
        {-# INLINE eat #-}

writeRanks :: FilePath -> U.Vector Rank -> IO ()
writeRanks filePath ranks
 = do   hFile   <- openFile filePath WriteMode
        go hFile 0
        hClose hFile

 where  go h !i
         | i >= U.length ranks
         = return ()

         | otherwise
         = do   hPutStr  h (show i ++ ": " ++ (show $ ranks U.! i))
                hPutChar h '\n'
                go h (i + 1)
