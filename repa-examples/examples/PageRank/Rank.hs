
module Rank 
        (rank)
where
import Page
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U
import qualified Data.IntMap            as M


-- | Compute PageRanks for these pages.
rank :: Int -> V.Vector Page -> U.Vector Rank
rank iters pages
 = go iters (initialRanks pages)
 where  
        -- Total number of pages.
        pageCount       = V.length pages        

        -- Step the algorithm the given number of times.
        go 0 ranks      = ranks
        go n ranks
         = go (n - 1) $ step pages ranks


-- | Compute new pageranks vector.
step    :: V.Vector Page        -- ^ Pages.
        -> U.Vector Rank        -- ^ Ranks of these pages.
        -> U.Vector Rank        

step pages ranks
 = let  
        -- The total number of pages.
        pageCount = V.length pages

        -- Get the rank map and score for all pages.
        (rankMaps, deadScores)
                  = unzip $ distribute pages ranks

        -- initial, empty map.
        map0      = M.fromList
                  $ [(r, 0) | r <- [0 .. U.length ranks - 1]]

        -- merge all the rank maps.
        rankMap   = mergeRankMaps $ V.fromList (map0 : rankMaps)

        -- Normalise the dead score by the total number of pages
        deadScore = sum deadScores
        deadRank  = deadScore / fromIntegral pageCount

        -- Convert the rank map back to a flat vector.
        ranks1    = U.map snd $ U.fromList $ M.toList rankMap

        -- Add in scores due to dead pages
        ranks2    = U.map (+ deadRank) ranks1

   in   ranks2


-- | Distribute processing of all these pages to the gang.
distribute
        :: V.Vector Page
        -> U.Vector Rank
        -> [ (M.IntMap Rank, Double) ]

distribute pages ranks
 = let  (rankMap, deadScore)
         = processPages pages ranks 

   in   [(rankMap, deadScore)]


-- | Process a packet of pages.
--   We compute scores assigned to other pages from these ones, 
--   as well as the total contribution to the dead score.
processPages 
        :: V.Vector Page        -- ^ Pages to assign scores from.
        -> U.Vector Rank        -- ^ Ranks of these pages
        -> ( M.IntMap Rank      --   Scores given to other pages.
           , Double)            --   Contribution to deadPages score

processPages pages ranks
 = let  rankMap = mergeRankMaps 
                $ V.zipWith spread (U.convert ranks) pages

        deadScore
                = U.sum
                $ U.zipWith 
                        (\flag rank -> if flag then rank else 0)
                        (U.convert $ V.map pageIsDangling pages)
                        ranks

   in    (rankMap, deadScore)


-- | Compute forward score given to other pages by this one.
spread :: Rank -> Page -> M.IntMap Rank
spread rank (Page pageId links)
 = U.foldl' insert M.empty links
 where  contrib 
         = rank / fromIntegral (U.length links)

        insert m link 
         = M.insertWith (+) link contrib m

