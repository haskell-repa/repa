
module Page
        ( -- * Pages.
          PageId
        , Page (..)
        , pageIsDangling
        , parsePage
        , parsePageId

          -- * Ranks
        , Rank
        , RankMap
        , mergeRankMaps
        , initialRanks)
where
import Data.Text
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U
import qualified Data.IntMap            as M
import qualified Data.Attoparsec.Text   as AT

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


parsePage :: Text -> Maybe Page
parsePage tx
 = case AT.parseOnly pPage tx of
        Right page        -> Just page
        _                 -> Nothing

 where  pPage
         = do   pageId  <- AT.decimal
                AT.char ':'
                AT.skipSpace
                links   <- AT.manyTill  
                           (do  x       <- AT.decimal
                                AT.skipSpace
                                return x)
                           AT.endOfInput
                return  $ Page pageId (U.fromList links)


parsePageId :: Text -> Maybe PageId
parsePageId tx
 = case AT.parseOnly pPageId tx of
        Right page      -> Just page
        _               -> Nothing

 where  pPageId
         = do   x       <- AT.decimal
                AT.char ':'
                return x

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


