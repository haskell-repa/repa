
module SolverChunks 
        ( quickHull
        , quickHull_minmax
        , quickHull_append
        , quickHull_dets)
where
import Data.Array.Repa.Vector.Segd              (Segd)
import Data.Array.Repa.Vector                   as R
import Data.Array.Repa.Vector.Index             as R
import Data.Array.Repa.Vector.Operators.Zip     as R
import Data.Array.Repa.Vector.Operators.Unzip   as R
import Data.Array.Repa.Vector.Operators.Append  as R
import Data.Array.Repa.Vector.Repr.Unboxed      as R
import Data.Array.Repa.Vector.Repr.Flow         as R
import qualified Data.Array.Repa.Vector.Segd    as Segd


-- | A point in the 2D plane.
type Point      = (Double, Double)


-- | Compute the convex hull of a vector of points.
quickHull :: Vector U Point -> Vector U Point
quickHull points
 = error "fark"
{-
 = let  -- Find the points that are on the extreme left and right.
        (minx, maxx)    = quickHull_minmax points

        -- Append all the points together.
        --  We'll find the hull of the top and bottom points at the same time.
        !n              = R.length points
        !downSegd       = Segd.fromLengths $ fromListUnboxed (Z :. 2) [n, n]
        !downPoints     = quickHull_append points
--        !downLines      = fromListUnboxed (Z :. 2) [(minx, maxx), (maxx, minx)]


        -- Compute the hull for the top and bottom of the plane.
        --  The results both sides are automatically concatenated.
        --  We don't need the final segment descriptor.
        hull            = downPoints

--        (_, !hull)      = hsplit_l downSegd downPoints downLines
   in   hull

-}

-- | Find the points on the extreme left and right of the XY plane.
quickHull_minmax :: Vector U Point -> (Point, Point)
quickHull_minmax vec
 | R.length vec == 0
 = error "no points"

 | otherwise
 = let  !p0     = vec `linearIndex` 0

        fmin p0@(!x0, !y0) p1@(!x1, !y1)
                = if x0 < x1   then p0 else p1

        fmax p0@(!x0, !y0) p1@(!x1, !y1)
                = if x0 > x1   then p0 else p1

        !minx   = R.fold fmin p0 vec
        !maxx   = R.fold fmax p0 vec
   in   (minx, maxx)
{-# NOINLINE quickHull_minmax #-}


-- | Append points together from the top and bottom part of the hull.
quickHull_append :: Vector U Point -> Vector U Point
quickHull_append !points
 = computeP $ R.append points points
{-# NOINLINE quickHull_append #-}


-- | Compute determinates between points and lines.
quickHull_dets 
        :: Segd
        -> Vector U Point
        -> Vector U (Point, Point)
        -> Vector U Double

quickHull_dets !segd !points !lines
 = R.unflowP
 $ R.zipWith detFn points
 $ (R.replicates segd lines :: Vector (O R.FD R.BB) (Point, Point))

 where  detFn xp@(!xo, !yo) ((!x1, !y1), (!x2, !y2))
         = (x1 - xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)
        {-# INLINE detFn #-}

