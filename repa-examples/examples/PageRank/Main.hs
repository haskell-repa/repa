{-# LANGUAGE BangPatterns #-}

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
import Page
import Prelude                                  as P
import Data.List
import System.IO

main
 = do   [pagesPath]      <- getArgs

        putStrLn $ "* Get maximum page id."
        pageCount       <- liftM (+ 1) $ getMaxPageId pagesPath
        putStrLn $ "  pages = " ++ show pageCount

        let startRank   = 1 / fromIntegral pageCount
        let ranks       = U.replicate pageCount startRank

        putStrLn $ "* Computing page ranks."
        run 1 pagesPath pageCount ranks


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
         = do   
                putStrLn $ "* Step " ++ show i
                !ranks1         <- UM.replicate pageCount 0
                (_, deadScore)  <- step pageFile pageCount ranks ranks1

                -- Normalise the deadScore by the total number of pages.
                let !deadRank   = deadScore / fromIntegral pageCount

                -- Add in scores due to dead pages.
                accDangling ranks1 deadRank

                -- Write the current ranks out to file.
                !ranks1  <- unsafeLiftIO $ U.unsafeFreeze ranks1
                let si  = replicate (2 - length (show i)) '0' ++ show i
                writeRanks ("out/step" ++ si ++ ".ranks") ranks1 

                go (i - 1) ranks1


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


-- | Compute new pageranks vector.
step    :: FilePath             -- ^ Pages file.
        -> Int                  -- ^ Total number of pages in the file.
        -> U.Vector Rank        -- ^ Old ranks.
        -> UM.IOVector Rank     -- ^ New ranks being computed.
        -> IO (Int, Rank)

step filePath pageCount ranks0 ranks1
 =  C.runResourceT
 $  B.sourceFile filePath
 $= B.lines
 $= T.decode T.utf8
 $$ C.foldM eat (0, 0)

 where  eat (nPages, deadScore) !line
         = nPages `seq` deadScore `seq`
           do   
                when (nPages `mod` 10000 == 0)
                 $ unsafeLiftIO $ print nPages

                -- Parse the line for this page.
                let Just page   = parsePage line

                -- Read the rank of the current page.
                let !rank       = ranks0 U.! (pageId page)

                -- Accumulate ranks given to other pages by this one.
                unsafeLiftIO 
                 $ accSpread ranks1 rank page

                -- If this page is dangling then add its rank to the deadScore
                let !deadScore'
                        | pageIsDangling page   = deadScore + rank
                        | otherwise             = deadScore

                return (nPages + 1, deadScore')


-- | Accumulate ranks into the Rank vector.
accRanks :: IOVector Rank 
         -> Map PageId Rank 
         -> IO ()

accRanks ranks m
 = P.mapM_ apply $ Map.toList m
 where apply (pageId, acc)
        = pageId `seq` acc `seq` 
          do    rank    <- UM.read ranks pageId
                UM.write ranks pageId (rank + acc)


-- | Accumulate forward score given from this page to others.
accSpread :: IOVector Rank
          -> Rank -> Page -> IO ()
accSpread ranks rank page
 = go 0
 where  !links   = pageLinks page
        !nLinks  = U.length links
        !contrib = rank / fromIntegral nLinks

        go !i
         | i >= nLinks
         = return ()

         | otherwise
         = do   let !pid = links U.! i
                r        <- UM.read ranks pid
                UM.write ranks pid (r + contrib)


-- | Accumulate ranks from dangling pages.
accDangling 
        :: IOVector Rank        -- ^ Ranks vector.
        -> Rank                 -- ^ Dangling rank
        -> IO ()
accDangling ranks danglingRank
 = go 0
 where go !i
        | i >= UM.length ranks  = return ()
        | otherwise
        = do    !r      <- UM.read ranks i
                UM.write ranks i (r + danglingRank)


-- | Compute forward score given to other pages by this one.
spread :: Rank -> Page -> Map PageId Rank
spread rank (Page pageId links)
 = U.foldl' insert Map.empty links
 where  !contrib 
         = rank / fromIntegral (U.length links)

        insert m link 
         = Map.insertWith (+) link contrib m


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

                when (n `mod` 10000 == 0)
                 $ unsafeLiftIO $ print n

                let !n' = max pid n
                return n'
        {-# INLINE eat #-}
