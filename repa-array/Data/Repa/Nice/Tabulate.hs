{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Nice.Tabulate
        ( tab
        , tabulate
        , Str (..)
        , Tok (..))
where
import Data.Repa.Nice.Present   as A
import Data.Repa.Nice.Display   as A
import Data.List                as L
import qualified Data.Text      as T
import Data.Text                (Text)
import Data.Monoid
import Data.Maybe


-- | Print a nested value to the console in tabular form.
--
--   The first two layers of nesting are displayed as rows and columns.
--   Numeric data is right-justified, while the rest is left-justified.
--
-- @
-- > tab [[10, 20, 302], [40, 50], [60, 7001, 80, 90 :: Int]]
-- 10   20 302
-- 40   50
-- 60 7001  80 90
-- @
--
--   Deeper layers of nesting are preserved in the output:
--
-- @
-- > tab [[[10], [20, 21]], [[30, 31], [40, 41, 41], [50 :: Int]]]
-- [10]    [20,21]   
-- [30,31] [40,41,41] [50]
-- @
--
--   By default, strings are printed as lists of characters:
--
-- @
-- > tab [[("red", 10), ("green", 20), ("blue", 30)], [("grey", 40), ("white", 50 :: Int)]]
-- ([\'r\',\'e\',\'d\'],10)     ([\'g\',\'r\',\'e\',\'e\',\'n\'],20) ([\'b\',\'l\',\'u\',\'e\'],30)
-- ([\'g\',\'r\',\'e\',\'y\'],40) ([\'w\',\'h\',\'i\',\'t\',\'e\'],50)
-- @
--
--  If you want double-quotes then wrap the strings with a @Str@ constructor:
--
-- @ 
-- > tab [[(Str "red", 10), (Str "green", 20), (Str "blue", 30)], [(Str "grey", 40), (Str "white", 50 :: Int)]]
-- ("red",10)  ("green",20) ("blue",30)
-- ("grey",40) ("white",50)
-- @
--
-- If you don't want any quotes then wrap them with a @Tok@ constructor:
--
-- @
-- > tab [[(Tok "red", 10), (Tok "green", 20), (Tok "blue", 30)], [(Tok "grey", 40), (Tok "white", 50 :: Int)]]
-- (red,10)  (green,20) (blue,30)
-- (grey,40) (white,50)
-- @
--
tab :: Presentable a => a -> IO ()
tab val
        = putStrLn $ T.unpack $ tabulate val


-- | Display a nested value in tabular form.
--
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

