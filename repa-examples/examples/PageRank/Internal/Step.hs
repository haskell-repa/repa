{-# LANGUAGE BangPatterns #-}
module Internal.Step
        (stepInternal)
where
import Page
import Progress
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM


-- | Perform one iteration step for the internal Page Rank algorithm.
stepInternal
        :: V.Vector Page        -- ^ Pages graph.
        -> U.Vector Rank        -- ^ Old ranks vector.
        -> IO (U.Vector Rank)   -- ^ New ranks vector.

stepInternal pages ranks
 = do   
        -- Create a new ranks vector full of zeros.
        let !pageCount  = V.length pages
        !ranks1         <- zeroRanks pageCount

        -- Add ranks contribution due to forward-links to the vector.
        dangleScore     <- accLinks pages ranks ranks1

        -- Normalise the dangleScore by the total number of pages.
        let !dangleRank   = dangleScore / fromIntegral pageCount

        -- Add in scores due to dangling pages.
        accDangling ranks1 dangleRank

        -- Freeze the ranks into an immutable vector.
        U.unsafeFreeze ranks1


zeroRanks :: Int -> IO (UM.IOVector Rank)
zeroRanks pageCount
 = UM.replicate pageCount 0
{-# NOINLINE zeroRanks #-}


-- | Add rank contributions due to forward-links to the ranks vector.
accLinks
        :: V.Vector Page                -- ^ Pages graph.
        -> U.Vector Rank                -- ^ Old ranks of the pages.
        -> UM.IOVector Rank             -- ^ New ranks being computed.
        -> IO Rank                      -- ^ Dangling score.

accLinks pages ranks0 ranks1
 = do   eatPages 0 0 
 where
        eatPages !ixPage !dangleScore
         | ixPage >= V.length pages
         = do   -- Print how many pages we've processed.
                printProgress "  pages eaten: " 10000 (ixPage + 1) (V.length pages + 1)
                return dangleScore

        eatPages !ixPage !dangleScore
         = do   -- Print how many pages we've processed.
                printProgress "  pages eaten: " 10000 (ixPage + 1) (V.length pages + 1)

                -- Get the current page.
                let !page       = pages  V.! ixPage

                -- Get the rank of the current page.
                let !rank       = ranks0 U.! pageId page

                -- Give scores to other pages that are linked to by this one.
                accSpread ranks1 rank page

                -- If this page is dangling then add its rank to the dangleScore.
                let !dangleScore'
                        | pageIsDangling page   = dangleScore + rank
                        | otherwise             = dangleScore

                eatPages (ixPage + 1) dangleScore'


-- | Accumulate forward score given from this page to others.
accSpread :: UM.IOVector Rank
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
                go (i + 1)


-- | Accumulate ranks from dangling pages.
accDangling 
        :: UM.IOVector Rank     -- ^ Ranks vector.
        -> Rank                 -- ^ Dangling rank
        -> IO ()

accDangling ranks danglingRank
 = go 0
 where go !i
        | i >= UM.length ranks  = return ()
        | otherwise
        = do    !r      <- UM.read ranks i
                UM.write ranks i (r + danglingRank)
                go (i + 1)

