
module SolverFlow where
import Data.Array.Repa.Vector                   as R
import Data.Array.Repa.Vector.Segd              as R
import Data.Array.Repa.Vector.Repr.Unboxed      as R

-- | A point in the 2D plane.
type Point      = (Double, Double)



hsplit_l 
        :: Segd
        -> Vector U Point
        -> Vector U (Point, Point)
        -> (Segd, Vector U Point)

hsplit_l segd points lines
 = let  -- The determinate tells us how far from its line each point is.
        !dets   = hsplit_det segd points lines 

        -- Select points above the lines.
        !above  = hsplit_above dets points

        -- Count how many points ended up in each segment.
        !counts = hsplit_count segd dets




   in   error "finish me"


-- Compute determinate of points.
hsplit_det 
        :: Segd 
        -> Vector U Point 
        -> Vector U (Point, Point) 
        -> Vector U (Point, Double)

hsplit_det !segd !points !lines
 = R.unflowP 
         $ R.zipWith detFn   (release points)
         $ R.replicates segd (release lines)
 where
        detFn xp@(xo, yo) ((x1, y1), (x2, y2))
         = (xp, (x1 - xo) * (y2 - yo) - (y1 - yo) * (x2 - xo))
        {-# INLINE detFn #-}
{-# NOINLINE hsplit_det #-}


-- Select points above the lines.
hsplit_above 
        :: Vector U Double 
        -> Vector U Point -> Vector U Point

hsplit_above !dets !points
 = R.unflowP
        $ R.pack 
        $ R.zip (R.map (> 0) (release dets)) (release points)
{-# NOINLINE hsplit_above #-}


-- Count how many points ended up in each segment
hsplit_count
        :: Segd
        -> Vector U Double -> Vector U Int

hsplit_count !segd !dets
 = R.unflowP
        $ R.counts (> 0) segd (release dets)
{-# NOINLINE hsplit_count #-}


-- 
hsplit_else 
        :: 
