{-# LANGUAGE BangPatterns #-}

import QuickHull
import Data.Array.Repa                  as R



main
 = do   let !points     = fromListUnboxed (Z :. 10)
                        $ [ (-4, 1), (-2, 1),  (-1,  4), ( 1,  1), (2,  3)
                          , ( 3, 2), (-4, -1), (-1, -2), (-2, -3), (1, -4)]

        hull    <- quickHull points

        print $ hull
