
module Page
        ( PageId
        , Page (..)
        , Rank
        , pageIsDangling
        , parsePage
        , parsePageId)
where
import Data.Text
import qualified Data.Vector.Unboxed    as U
import qualified Data.Attoparsec.Text   as AT


-- | Unique identifier for a page.
type PageId     
        = Int

data Page       
        = Page 
        { pageId        :: !PageId 
        , pageLinks     :: !(U.Vector PageId) }
        deriving Show

-- | A single PageRank value.
type Rank       
        = Double

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
         = do   pid  <- AT.decimal
                _    <- AT.char ':'
                AT.skipSpace
                links   <- AT.manyTill  
                           (do  x       <- AT.decimal
                                AT.skipSpace
                                return x)
                           AT.endOfInput
                return  $ Page pid (U.fromList links)


parsePageId :: Text -> Maybe PageId
parsePageId tx
 = case AT.parseOnly pPageId tx of
        Right page      -> Just page
        _               -> Nothing

 where  pPageId
         = do   x       <- AT.decimal
                _       <- AT.char ':'
                return x
