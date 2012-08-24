
module Internal.Step
        (stepInternal)
where
import Page
import Progress
import Control.Monad
import Data.IORef
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import qualified Data.Array.Repa.Eval.Gang      as R


-- | Perform one iteration step for the internal Page Rank algorithm.
stepInternal
        :: V.Vector Page        -- ^ Pages graph.
        -> U.Vector Rank        -- ^ Old ranks vector.
        -> IO (U.Vector Rank)   -- ^ New ranks vector.

stepInternal pages !ranks
 = do   
        -- Create a new ranks vector full of zeros.
        let !pageCount  = V.length pages
        !ranks1         <- zeroRanks pageCount

        -- Add ranks contribution due to forward-links to the vector.
        !dangleScore    <- accLinks pages ranks ranks1

        -- Normalise the dangleScore by the total number of pages.
        let !dangleRank = dangleScore / fromIntegral pageCount

        -- Add in scores due to dangling pages.
        accDangling ranks1 dangleRank

        -- Freeze the ranks into an immutable vector.
        !final          <- U.freeze ranks1
        return final


-- | Create a zero valued dense ranks vector.
zeroRanks :: Int -> IO (UM.IOVector Rank)
zeroRanks pageCount
 = UM.replicate pageCount 0
{-# NOINLINE zeroRanks #-}
--  NOINLINE so we can see the allocation this function does when profiling.


-- | Add ranks contributions due to forward-links to the ranks vector.
accLinks
        :: V.Vector Page        -- ^ Pages graph.
        -> U.Vector Rank        -- ^ Old ranks of the pages.
        -> UM.IOVector Rank     -- ^ New ranks of the pages.
        -> IO Rank              -- ^ Dangling score.

accLinks pages ranks0 ranks1
 = do   
        -- Create new refs to hold the partial results computed
        -- by each thread.
        let !threads    = R.gangSize R.theGang
        refsRank        <- replicateM threads (newIORef Nothing)
        refsDangles     <- replicateM threads (newIORef Nothing)

        -- Send work requests to each thread.
        R.gangIO R.theGang
         (\nThread -> accLinksThread pages ranks0 
                        (refsRank    !! nThread) 
                        (refsDangles !! nThread)
                        nThread)

        -- Read the results back from the refs.
        Just ranks      <- liftM sequence $ mapM readIORef refsRank
        Just dangles    <- liftM sequence $ mapM readIORef refsDangles

        -- Add each thread's contributions to the overall ranks vector.
        mapM_ (accRanks ranks1) ranks
        let  !dangleScore = sum dangles

        return dangleScore


-- | Add a ranks vector to the mutable accumulator.
accRanks :: UM.IOVector Rank -> U.Vector Rank -> IO ()
accRanks !dest !ranks
 = go 0
 where  go !ix
         | ix >= U.length ranks
         = return ()

         | otherwise
         = do   x       <- UM.unsafeRead dest ix
                UM.unsafeWrite dest ix (x + U.unsafeIndex ranks ix)
                go (ix + 1)


-- | Sum up rank contributions due to out-links for the pages assigned
--   to a single thread.
accLinksThread 
        :: V.Vector Page                 -- ^ Pages graph.
        -> U.Vector Rank                 -- ^ Old ranks of the pages.
        -> IORef (Maybe (U.Vector Rank)) -- ^ Write ranks to this ref.
        -> IORef (Maybe Rank)            -- ^ Write dangle score to this ref.
        -> Int                           -- ^ Thread number.
        -> IO ()

accLinksThread pages ranks refRank1 refDangleScore nThread
 = do   
        -- Allocate the buffer that this thread will write its
        -- contributions into.
        let !pageCount  = V.length pages
        !ranks1         <- zeroRanks pageCount

        -- Get the starting and ending indices of the pages that
        -- this thread will process.
        let !ixStart    = nstart pageCount nThread
        let !ixEnd      = nstart pageCount (nThread + 1)

        -- Accumulate this thread's slice of pages into the buffer.
        !dangleScore    <- accLinksRange False ixStart ixEnd pages ranks ranks1

        -- Send the results back to the master thread.
        !vec'           <- U.unsafeFreeze ranks1
        writeIORef refRank1       (Just vec')
        writeIORef refDangleScore (Just dangleScore)


-- | Add rank contributions due to out-links to the ranks vector.
accLinksRange
        :: Bool
        -> Int                          -- ^ Starting index.
        -> Int                          -- ^ Index after the last one to add.
        -> V.Vector Page                -- ^ Pages graph.
        -> U.Vector Rank                -- ^ Old ranks of the pages.
        -> UM.IOVector Rank             -- ^ New ranks being computed.
        -> IO Rank                      -- ^ Dangling score.

accLinksRange verbose ixStart ixEnd pages ranks0 ranks1
 = do   eatPages ixStart 0 
 where
        eatPages !ixPage !dangleScore
         | ixPage >= ixEnd
         = do   -- Print how many pages we've processed.
                when verbose
                 $ printProgress "  pages eaten: " 10000
                        (ixPage + 1)
                        (ixEnd + 1)
                return dangleScore

        eatPages !ixPage !dangleScore
         = do   -- Print how many pages we've processed.
                when verbose
                 $ printProgress "  pages eaten: " 10000
                        (ixPage + 1)
                        (ixEnd  + 1)

                -- Get the current page.
                let !page       = pages  V.! ixPage

                -- Get the rank of the current page.
                let !rank       = ranks0 U.! (fromIntegral $ pageId page)

                -- Give scores to other pages that are linked to by this one.
                accSpread ranks1 rank page

                -- If this page is dangling then add its rank to the dangleScore.
                let !dangleScore'
                        | pageIsDangling page   = dangleScore + rank
                        | otherwise             = dangleScore

                eatPages (ixPage + 1) dangleScore'


-- | Get the starting point for a chunk.
nstart  :: Int -> Int -> Int
nstart !len !c
 = let  chunks          = R.gangSize R.theGang
        chunkLen        = len `quot` chunks
        chunkLeftover   = len `rem`  chunks

        getStart c'
         | c' < chunkLeftover  = c' * (chunkLen + 1)
         | otherwise           = c' * chunkLen  + chunkLeftover

  in    getStart c


-- | Accumulate forward score given from this page to others.
accSpread :: UM.IOVector Rank
          -> Rank -> Page -> IO ()

accSpread !ranks !rank !page
 = go 0
 where  !links   = pageLinks page
        !nLinks  = U.length links
        !contrib = rank / fromIntegral nLinks

        go !i
         | i >= nLinks
         = return ()

         | otherwise
         = do   let !pid = links U.! i
                r        <- UM.read ranks (fromIntegral pid)
                UM.write ranks (fromIntegral pid) (r + contrib)
                go (i + 1)


-- | Accumulate ranks from dangling pages.
accDangling 
        :: UM.IOVector Rank     -- ^ Ranks vector.
        -> Rank                 -- ^ Dangling rank
        -> IO ()

accDangling !ranks !danglingRank
 = go 0
 where go !i
        | i >= UM.length ranks  = return ()
        | otherwise
        = do    !r      <- UM.read ranks i
                UM.write ranks i (r + danglingRank)
                go (i + 1)

