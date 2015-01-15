{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
module Data.Repa.Nice.Tabulate
        (tabulate)
where
import Data.Repa.Nice.Present   as A
import Data.Repa.Nice.Display   as A
import Data.Repa.Nice           as A
import Data.List                as L
import Data.Monoid
import Data.Maybe


-- | Display a value in tabular form.
tabulate :: Presentable a => a -> String
tabulate xx
  = let pp      = present xx
    in case depth pp of
        0       -> flatten pp

        1       -> let Just pss = strip1 pp
                   in  tabulate1 $ map flatten pss

        _       -> let Just pss = strip2 pp
                   in  tabulate2 $ map (map flatten) pss

tabulate1 :: [String] -> String
tabulate1 strs
  = let lens    = L.map L.length strs
        len     = maximum lens
    in  concatMap (padR (len + 1)) strs


tabulate2 :: [[String]] -> String
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
         = L.intercalate " " $ L.zipWith display displays line

    in  L.intercalate "\n" $ L.map makeLine strss

