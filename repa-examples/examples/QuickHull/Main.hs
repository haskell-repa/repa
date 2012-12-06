{-# LANGUAGE BangPatterns #-}
import Solver
import SVG
import Points
import Data.Array.Repa.Vector                   as R
import qualified Data.Vector.Unboxed            as U
import Data.Array.Repa.IO.Timing

main
 = do
        let pointCount  = 10000
        let uPoints     = genPointsDisc pointCount (400, 400) 350 
        let vPoints     = R.fromUnboxed (Z :. pointCount) uPoints
        let mFileSVG    = Nothing -- Just "out.svg"

        -- Force points to create the input vector.
        U.force uPoints `seq` return ()

        -- Compute the convex hull.
        (vHull, tElapsed)
                <- time 
                $  let  vHull   = quickHull vPoints
                   in   vHull `seq` return vHull
                                        
        -- Print how long it took.
        putStr $ prettyTime tElapsed

        let !uHull       = R.toUnboxed vHull

        -- If we were asked for an SVG then write it out to file.
        maybe   (return ())
                (\fileSVG -> 
                        writeFile fileSVG
                         $ makeSVG      (roundPoints $ U.toList uPoints) 
                                        (roundPoints $ U.toList uHull))
                mFileSVG

{-
points1
 = fromListUnboxed (Z :. 10)
        $ [ (-4, 1), (-2, 1),  (-1,  4), ( 1,  1), (2,  3)
          , ( 3, 2), (-4, -1), (-1, -2), (-2, -3), (1, -4)]
-}
