{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
module Data.Repa.Nice.Tabulate
        (tabulate)
where
import Data.Repa.Nice.Present   as A
import Data.Repa.Nice.Display   as A
import Data.List                as L
import qualified Data.Text      as T
import Data.Text                (Text)
import Data.Monoid
import Data.Maybe


-- | Display a value in tabular form.
tabulate :: Presentable a => a -> Text
tabulate xx
  = let pp      = present xx
    in case depth pp of
        0       -> flatten pp

        1       -> let Just pss = strip1 pp
                   in  tabulate1 $ map flatten pss

        _       -> let Just pss = strip2 pp
                   in  tabulate2 $ map (map flatten) pss

tabulate1 :: [Text] -> Text
tabulate1 strs
  = let d       = mconcat $ map takeDisplay strs
    in  T.intercalate (T.pack "\n") 
         $ L.map (display d) strs


tabulate2 :: [[Text]] -> Text
tabulate2 strss
 = let 
        -- Decide how to display a single column.
        displayOfCol c
                = mconcat
                $ mapMaybe (\line -> if c >= length line
                                        then Nothing
                                        else Just (takeDisplay (line !! c)))
                $ strss

        -- How many columns we have.
        nCols    = maximum $ L.map L.length strss

        -- Decide how to display all the columns.
        displays = L.map displayOfCol [0.. nCols - 1]

        makeLine line
         = T.intercalate (T.pack " ") 
         $ L.zipWith display displays line

    in  T.intercalate (T.pack "\n") 
         $ L.map makeLine strss

