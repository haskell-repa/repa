{-# LANGUAGE BangPatterns #-}
module Solver
        (quickHull)
where
import Data.Array.Repa                          as R
import Data.Array.Repa.Vector                   as R
import Prelude                                  as P
import Data.Array.Repa.Vector.Segd              (Segd(..))
import qualified Data.Array.Repa.Vector.Segd    as Segd

type Point      = (Int, Int)


quickHull :: Vector U Point -> IO (Vector U Point)
quickHull points
 = do   let !n          = vlength points
        
        -- Find the points that are on the extreme left and right.
        (minx, maxx)    <- minmax points
        putStrLn $ "min max = " P.++ show (minx, maxx)

        -- Append the points together. 
        -- We'll find the hull on the top and bottom at the same time.
        !psFlat         <- R.computeUnboxedP $ R.append points points
        putStrLn $ "psFlat  = " P.++ show psFlat

        let !segd       = Segd  (R.fromListUnboxed (Z :. 2) [n, n])
                                (R.fromListUnboxed (Z :. 2) [0, n])
                                (n + n)

        -- Compute the hull for the top and bottom.
        -- The results from both sides are automatically concatenated.
        hulls_points    <- hsplit_l segd psFlat 
                        $  R.fromListUnboxed (R.Z R.:. (2 :: Int))
                                [ (minx, maxx), (maxx, minx) ]

        return $ error "quickHull: finish me"


-- | Find the points on the extreme left and right of the XY plane.
minmax :: Vector U Point -> IO (Point, Point)
minmax vec
 | vlength vec == 0
 = error "no points"

 | otherwise
 = do   let p0  = vec `R.unsafeIndex` (R.Z R.:. 0)

        let fmin p0@(x0, y0) p1@(x1, y1)
                = if x0 < x1   then p0 else p1

        let fmax p0@(x0, y0) p1@(x1, y1)
                = if x0 > x1   then p0 else p1

        !minx   <- R.foldAllP fmin p0 vec 
        !maxx   <- R.foldAllP fmax p0 vec
        return (minx, maxx)
{-# NOINLINE minmax #-}


hsplit_l :: Segd U U                    -- Descriptor for points array.
         -> Vector U Point              -- Segments of points.
         -> Vector U (Point, Point)     -- Splitting lines for each segment.
         -> IO (Vector U Point)         -- Hull points for all segments.

hsplit_l upsegd points lines
 = error "hsplit_l: finish me"


-- Take the dot product between the splitting line and each point
-- to tell us what side of the line they are on.
hsplit_dot
        :: Segd                         -- Descriptor for points array.
        -> Vector U Point               -- Segments of points.
        -> Vector U (Point, Point)      -- Splitting lines for each segment.
        -> Vector U Double              -- Cross products for each point.

hsplit_dot !segd !points !lines 
 = vcomputeUnboxedP 
 $ vzipWith
        cross_fn1
        points
        (vreplicates segd lines)

 where  cross_fn1 (xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) 
         - (y1 - yo) * (x2 - xo)
        {-# INLINE cross_fn1 #-}
{-# NOINLINE hsplit_dot #-}
