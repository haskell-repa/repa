{-# LANGUAGE BangPatterns #-}

module External.Step
        (stepExternal)
where
import Page
import Data.Vector.Unboxed.Mutable              (IOVector)
import qualified Data.ByteString.Lazy.Char8     as BL
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Progress


-- | Perform one iteration step for the Page Rank algorithm.
stepExternal
        :: FilePath             -- ^ Pages file.
        -> Int                  -- ^ Total number of lines in the file.
        -> Int                  -- ^ Total number of pages.
        -> U.Vector Rank        -- ^ Old ranks vector.
        -> IO (U.Vector Rank)   -- ^ New ranks vector.

stepExternal pageFile lineCount pageCount ranks
 = do
        -- Create a new ranks vector full of zeros.
        !ranks1            <- zeroRanks pageCount

        -- Add ranks contributions due to forward-links to the vector.
        deadScore  <- accLinks pageFile lineCount pageCount ranks ranks1
        printProgress "  lines read: " 10000 lineCount lineCount

        -- Normalise the deadScore by the total number of pages.
        let !deadRank   = deadScore / fromIntegral pageCount

        -- Add in scores due to dead pages.
        accDangling ranks1 deadRank

        -- Freeze the ranks into an immutable vector.
        U.unsafeFreeze ranks1


zeroRanks :: Int -> IO (UM.IOVector Rank)
zeroRanks pageCount
 = UM.replicate pageCount 0
{-# NOINLINE zeroRanks #-}


-- | Add rank contributions due to forward-links to a ranks vector.
accLinks
        :: FilePath             -- ^ Pages file.
        -> Int                  -- ^ Number of lines in the file.
        -> Int                  -- ^ Total number of pages.
        -> U.Vector Rank        -- ^ Old ranks of the pages.
        -> UM.IOVector Rank     -- ^ New ranks being computed.
        -> IO Rank

accLinks filePath lineCount _pageCount ranks0 ranks1
 = do   bs      <- BL.readFile filePath
        eatLines (0, 1, 0) (BL.lines bs)

 where  eatLines (_, _, deadScore) []
         = return deadScore

        eatLines (!ixLine, !ixPage, !deadScore) (!l : ls)
         = do   -- Print how far along we are.
                printProgress "  lines read: " 10000 ixLine lineCount

                -- Parse the line for this page.
                let Just page   = parsePage l

                -- Accumulate data from this page.
                eatPage ixLine ixPage deadScore page ls

        eatPage !ixLine !ixPage !deadScore !page !ls
         -- Ok, we read the page we were expecting.
         | pageId page == ixPage
         = do   -- Get the rank of the current page.
                let !rank       = ranks0 U.! pageId page

                -- Give scores to other pages that are linked to by this one.
                accSpread ranks1 rank page

                -- If this page is dangling then add its rank to the deadScore
                let !deadScore'
                        | pageIsDangling page   = deadScore + rank
                        | otherwise             = deadScore

                eatLines (ixLine + 1, ixPage + 1, deadScore') ls

         -- The page id was higher than what we were expecting.
         -- We've skipped over some page with no out-links that was not
         -- mentioned in the source file.
         | pageId page >= ixPage
         = do   -- Get the rank of the expected page.
                let !rank       = ranks0 U.! ixPage

                -- This page is dangling because it had no out-links.
                let deadScore'  = deadScore + rank
                eatPage ixLine (ixPage + 1) deadScore' page ls

         -- If the page id read from the file is less than what 
         -- we were expecting then the links file isn't sorted.
         | otherwise
         = error $ "accLinks: page ids in links file are not monotonically increasing"


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
                go (i + 1)


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
                go (i + 1)

