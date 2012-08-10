
module Page
        ( -- * Pages.
          PageId
        , Page (..)
        , pageIsDangling

          -- * Ranks
        , Rank
        , RankMap
        , mergeRankMaps
        , initialRanks)
where
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U
import qualified Data.IntMap            as M


-- Page -----------------------------------------------------------------------
-- | Unique identifier for a page.
type PageId     
        = Int

data Page       
        = Page 
        { pageId        :: !PageId 
        , pageLinks     :: !(U.Vector PageId) }
        deriving Show


-- | Check if a page has no out-links.
pageIsDangling :: Page -> Bool
pageIsDangling page
        = U.length (pageLinks page) == 0


-- Ranks ----------------------------------------------------------------------
-- | A single PageRank value.
type Rank       
        = Double

-- | Map of PageIds to PageRanks.
type RankMap    
        = M.IntMap Rank


-- | Merge a vector of RankMaps.
mergeRankMaps   :: V.Vector RankMap -> RankMap
mergeRankMaps rankMaps
        = V.foldl' (M.unionWith (+)) M.empty rankMaps


-- | Create initial ranks for these pages.
initialRanks :: V.Vector Page -> U.Vector Rank
initialRanks pages
 = let  pageCount       = V.length pages
   in   U.replicate pageCount (1 / fromIntegral pageCount)
