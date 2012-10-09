{-# LANGUAGE BangPatterns #-}
module QuickHull where
import Data.Array.Repa                                          (U, Z, (:.))
import Data.Vector.Repa
import Data.Array.Parallel.Unlifted.Parallel.UPSegd             (UPSegd)
import qualified Data.Array.Repa                                as R
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as US
import Data.Function

type Point      = (Double, Double)

quickHull :: Vector U Point -> IO (Vector U Point)
quickHull points
 = do   let !n          = vlength points
        
        -- Find the points that are on the extreme left and right.
        (minx, maxx)    <- minmax points

        putStrLn $ "min max = " ++ show (minx, maxx)

        -- Append the points together, 
        -- we'll determine the hull on the top and bottom at the same time.
        !psflat         <- R.computeP $ R.append points points
        let !upsegd     =  UPSegd.fromLengths 
                        $  US.fromList [n, n]

        putStrLn $ "psflat  = " ++ show psflat
        putStrLn $ "upsegd  = " ++ show (UPSegd.takeLengths upsegd)

        -- Compute the hull for the top and bottom.
        -- The results from both sides are automatically concatenated.
        hulls_points    <- hsplit_l upsegd psflat 
                        $  R.fromListUnboxed (R.Z R.:. (2 :: Int))
                                [ (minx, maxx), (maxx, minx) ]

        -- Concatentate hull points by just returning the flat vector.
        return hulls_points


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


hsplit_l :: UPSegd -> Vector U Point    -- Segments of points.
         -> Vector U (Point, Point)     -- Splitting lines for each segment.
         -> IO (Vector U Point)         -- Hull points for all segments.

hsplit_l upsegd points lines
 = do   
        -- Select only the points on the positive side of the line.
        let !cross      =  hsplit_cross upsegd points lines

        !flags          <- R.computeUnboxedP $ R.map (> 0) cross

        putStrLn $ "cross   = " ++ show cross
        putStrLn $ "flags   = " ++ show flags

        return $ error "finish me"
{-# NOINLINE hsplit_l #-}


-- | Only want one replicates with the same usegd per zipWith
--   so we don't duplicate the code that keeps track of what
--   segment we're up to.
hsplit_cross
        :: UPSegd -> Vector U Point     -- Segments of points.
        -> Vector U (Point, Point)      -- Splitting lines for each segment.
        -> Vector U Double              -- Cross products for each point.

hsplit_cross !upsegd !points !lines 
 = unchainP
 $ vzipWith
        cross_fn1
        points
        (vreplicates upsegd lines)

 where  cross_fn1 (xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) 
         - (y1 - yo) * (x2 - xo)
        {-# INLINE cross_fn1 #-}
{-# NOINLINE hsplit_cross #-}

