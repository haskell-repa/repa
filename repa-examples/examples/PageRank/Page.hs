
module Page
        ( PageId
        , Page (..)
        , Rank
        , pageIsDangling
        , parsePage
        , parsePageId)
where
import qualified Data.ByteString.Lazy.Char8     as BL
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad.ST


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
        = Float

-- | Check if a page has no out-links.
pageIsDangling :: Page -> Bool
pageIsDangling page
        = U.length (pageLinks page) == 0


parsePageId :: BL.ByteString -> Maybe PageId
parsePageId bs
        | Just (pid, _)         <- BL.readInt bs
        = Just pid

        | otherwise
        = Nothing


parsePage :: BL.ByteString -> Maybe Page
parsePage bs
        | Just (pid, bs2)       <- BL.readInt bs
        , Just bs3              <- char ':' bs2
        , (links, _)            <- ints bs3
        = Just (Page pid links)

        | otherwise
        = Nothing


char   :: Char -> BL.ByteString -> Maybe BL.ByteString
char c bs
 | BL.null bs           = Nothing
 | BL.head bs == c      = Just (BL.tail bs)
 | otherwise            = Nothing
{-# INLINE char #-}


ints    :: BL.ByteString -> (U.Vector Int, BL.ByteString)
ints bs0
 = runST
 $ do   mvec    <- UM.new 100
        go mvec 0 100 bs0

 where  go mvec ix ixMax bs
         | ix >= ixMax
         = do   mvec'   <- UM.grow mvec ixMax
                go mvec' ix (2 * ixMax) bs

         | BL.null bs
         = do   vec     <- U.freeze (UM.slice 0 ix mvec)
                return (vec, bs)

         | Just bs2     <- char ' ' bs
         = go mvec ix ixMax bs2

         | Just (i, bs2) <- BL.readInt bs
         = do   UM.write mvec ix i
                go mvec (ix + 1) ixMax bs2

         | otherwise
         = do   vec     <- U.freeze (UM.slice 0 ix mvec)
                return (vec, bs)
