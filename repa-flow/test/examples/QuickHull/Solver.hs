{-# LANGUAGE MagicHash, BangPatterns #-}
module Solver where
import Data.Vector.Unboxed
import Data.Array.Repa.Flow             (Flow)
import qualified Data.Array.Repa.Flow   as F
import qualified Data.Vector.Unboxed    as U
import GHC.Exts

type Point = (Double, Double)


hsplit_det
        :: Vector Int                   -- ^ Lengths for points array
        -> Vector Point                 -- ^ Segments of points.
        -> Vector (Point, Point)        -- ^ Splitting lines for each segment.
        -> IO (Vector Double)           -- ^ Determiniates for points.

hsplit_det !vLens !vPoints !vLines
 = do   
        let !total       = U.sum vLens
        !fPoints         <- F.flow vPoints
        !fLinesRep       <- F.replicatesUnboxed total vLens vLines
        F.unflow $ F.zipWith detFn fPoints fLinesRep

 where  detFn (xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) 
         - (y1 - yo) * (x2 - xo)
        {-# INLINE detFn #-}
{-# NOINLINE hsplit_det #-}
