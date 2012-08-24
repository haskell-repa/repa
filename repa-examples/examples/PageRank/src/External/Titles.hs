
module External.Titles
        ( slurpTopRanks
        , mergeRanks)
where
import Progress
import Page
import System.IO
import Data.IORef
import qualified Data.Vector.Algorithms.Heap    as VA
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector                    as V
import qualified Data.IntMap                    as M
import qualified Data.ByteString.Lazy.Char8     as BL


-- | Given the dense rank vector, slurp out the page ids and ranks for
--   some of the highest ranked pages.
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
                $ map (\(pid, x) -> (fromIntegral pid, x))
                $ U.toList ranks

        -- Read titles and add to this ref.
        !outList        <- collectTitles titlesPath mm

        -- Sort the resulting titles.
        !outVec'        <- V.thaw $ V.fromList outList      
        VA.sortBy compareRanks outVec'
        !outVec_sorted  <- V.unsafeFreeze outVec'

        -- Write out to file.
        V.mapM_ (\(pid, rank, title)
                -> hPutStrLn hOut
                        $  padR 10 (show pid) 
                        ++ " "
                        ++ padL 18 (show rank)
                        ++ ": " 
                        ++ BL.unpack title)
            outVec_sorted

        hClose hOut


compareRanks 
        :: (PageId, Rank, BL.ByteString)
        -> (PageId, Rank, BL.ByteString)
        -> Ordering
compareRanks (_, r1, _) (_, r2, _)
 = compare r2 r1
{-# INLINE compareRanks #-}


-- | For all the pages in the given map,
--   read the pages title from the titles file,
--   and return a list of all the pages with their ranks and titles.
collectTitles 
        :: FilePath             -- ^ Path to titles file.
        -> M.IntMap Rank        -- ^ Map of PageId to its rank.
        -> IO [(PageId, Rank, BL.ByteString)]

collectTitles !titlesPath !mm
 = do   outRef   <- newIORef []
        bsTitles <- BL.readFile titlesPath
        go outRef 1 (BL.lines bsTitles)
        readIORef outRef

 where  go _ _ [] 
         = return ()

        go outRef !pid (!title : rest)
         = case M.lookup (fromIntegral pid) mm of
            Nothing     
             ->    go outRef (pid + 1) rest

            Just rank
             -> do out <- readIORef outRef
                   writeIORef outRef ((pid, rank, title) : out)
                   go outRef (pid + 1) rest
