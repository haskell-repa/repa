{-# LANGUAGE BangPatterns #-}

module Step
        (step)
where
import Page
import Data.Conduit.List                        as C
import Data.Conduit.Text                        as T
import Data.Conduit                             as C
import Data.Conduit.Binary                      as B
import Data.Vector.Unboxed.Mutable              (IOVector)
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Progress

-- | Perform one iteration step for the Page Rank algorithm.
step    :: FilePath             -- ^ Pages file.
        -> Int                  -- ^ Total number of lines in the file.
        -> Int                  -- ^ Total number of pages.
        -> U.Vector Rank        -- ^ Old ranks vector.
        -> IO (U.Vector Rank)   -- ^ New ranks vector.

step pageFile lineCount pageCount ranks
 = do
        -- Create a new ranks vector full of zeros.
        !ranks1         <- UM.replicate pageCount 0

        -- Add ranks contributions due to forward-links to the vector.
        (_, deadScore)  <- accLinks pageFile lineCount pageCount ranks ranks1
        printProgress "  lines read: " 10000 lineCount lineCount

        -- Normalise the deadScore by the total number of pages.
        let !deadRank   = deadScore / fromIntegral pageCount

        -- Add in scores due to dead pages.
        accDangling ranks1 deadRank

        -- Freeze the ranks into an immutable vector.
        unsafeLiftIO $ U.unsafeFreeze ranks1


-- | Add rank contributions due to forward-links to a ranks vector.
accLinks
        :: FilePath             -- ^ Pages file.
        -> Int                  -- ^ Number of lines in the file.
        -> Int                  -- ^ Total number of pages.
        -> U.Vector Rank        -- ^ Old ranks of the pages.
        -> UM.IOVector Rank     -- ^ New ranks being computed.
        -> IO (Int, Rank)

accLinks filePath lineCount _pageCount ranks0 ranks1
 =  C.runResourceT
 $  B.sourceFile filePath
 $= B.lines
 $= T.decode T.utf8
 $$ C.foldM eat (0, 0)

 where  eat (nLines, deadScore) !line
         = nLines `seq` deadScore `seq`
           unsafeLiftIO $ do
                -- Print how far along we are.
                printProgress "  lines read: " 10000 nLines lineCount

                -- Parse the line for this page.
                let Just page   = parsePage line

                -- Read the rank of the current page.
                let !rank       = ranks0 U.! (pageId page)

                -- Accumulate ranks given to other pages by this one.
                accSpread ranks1 rank page

                -- If this page is dangling then add its rank to the deadScore
                let !deadScore'
                        | pageIsDangling page   = deadScore + rank
                        | otherwise             = deadScore

                return (nLines + 1, deadScore')

                
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



