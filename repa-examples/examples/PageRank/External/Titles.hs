{-# LANGUAGE BangPatterns #-}
module External.Titles
        (mergeRanks)
where
import Progress
import Page
import System.IO
import Data.IORef
import Data.Text                                as T
import qualified Data.Vector.Algorithms.Heap    as VA
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector                    as V
import qualified Data.IntMap                    as M
import qualified Data.ByteString.Lazy.Char8     as BL


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
