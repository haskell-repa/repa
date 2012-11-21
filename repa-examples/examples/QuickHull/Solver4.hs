
module SolverFlow where
import Data.Array.Repa.Vector.Segd              (Segd)
import Data.Array.Repa.Vector                   as R
import Data.Array.Repa.Vector.Operators.Zip     as R
import Data.Array.Repa.Vector.Operators.Unzip   as R
import Data.Array.Repa.Vector.Repr.Unboxed      as R
import qualified Data.Array.Repa.Vector.Segd    as Segd

-- | A point in the 2D plane.
type Point      = (Double, Double)



hsplit_l 
        :: Segd
        -> Vector U Point
        -> Vector U (Point, Point)
        -> (Segd, Vector U Point)

hsplit_l segd points' lines'
 = let  points  = release points'
        lines   = release lines'

        -- The determinate tells us how far from its line each point is.
        dets    :: Vector U Double
        !dets   = release $ R.unflowP
                $ R.zipWith detFn   points
                $ R.replicates segd lines

        detFn xp@(xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)
        {-# INLINE detFn #-}

        -- Select points above the lines.
        above   :: Vector U Point
        !above  = release $ R.unflowP 
                $ R.pack
                $ R.zip (R.map (> 0) dets) points

        -- Count how many points ended up in each segment.
        counts  :: Vector U Int
        !counts = release $ R.unflowP
                $ R.counts (> 0) segd dets

        flagsIf :: Vector D Bool
        !flagsIf = R.map (> 0) counts

        -- if-then-else ------------------------------------ ELSE
        lines_else      = R.pack $ R.zip flagsIf lines
        counts_else     = R.pack $ R.zip flagsIf counts
        lengths_else    = R.pack $ R.zip flagsIf (Segd.lengths segd)


   in   error "finish me"

