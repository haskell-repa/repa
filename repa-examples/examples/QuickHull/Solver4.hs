
module SolverFlow where
import Data.Array.Repa.Vector                   as R
import Data.Array.Repa.Vector.Segd              as R
import Data.Array.Repa.Vector.Repr.Unboxed      as R

-- | A point in the 2D plane.
type Point      = (Double, Double)


-- Compute determinate of points.
hsplit_det
        :: Segd                         -- ^ Lengths for points array
        -> Vector U Point               -- ^ Segments of points.
        -> Vector U (Point, Point)      -- ^ Splitting lines for each segment.
        -> Vector U Double              -- ^ Determiniates for points.

hsplit_det segd points lines
 = let  points'         = release points
        lines'          = release lines

        detFn (xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) 
         - (y1 - yo) * (x2 - xo)
        {-# INLINE detFn #-}

   in   R.unflowP 
         $ R.zipWith detFn points'
         $ R.replicates segd lines'
{-# NOINLINE hsplit_det #-}


-- Select points above the lines.
hsplit_above
        :: Vector U Double              -- ^ Determinates of points
        -> Vector U Point               -- ^ Segments of points.
        -> Vector U Point               -- ^ Points that lie above the lines.

hsplit_above dets points
 = do   
        -- Select points above the lines.
        !fDets   <- F.flow vDets
        !fPoints <- F.flow vPoints
        !fAbove  <- F.pack (F.zip (F.map (> 0) fDets) fPoints)
                                -- ^ Want to use delayed arrays on the left of the zip.

        !vAbove  <- F.unflow fAbove

        return vAbove        
{-# NOINLINE hsplit_above #-}
