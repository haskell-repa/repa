{-# LANGUAGE MagicHash, BangPatterns #-}
module Solver where
import Data.Vector.Unboxed
import Data.Array.Repa.Flow             (Flow)
import qualified Data.Array.Repa.Flow   as F
import qualified Data.Vector.Unboxed    as U
import GHC.Exts

type Point = (Double, Double)


-- When making the repa-vector wrapper,
--      store the distribution in the representation tag.
--      Don't need to distinguish Stream and Chain as whether they 
--      have the skip constructor, but do want to record balancing, 
--      and information about the expected length.
--
-- Functions like map and zip are all pure,
-- each to make ve

-- make vmap    etc as the pure vector versions.
--      ofilter etc as the incremental flow versions.

-- Just start new repa-vector style classes in the repa-flow dir.
--
--      Vector (L O) Int                -- Balanced flow
--      Vector (N O) Int 
--
-- Can make the Dist thing a type function that wraps the inner representation.
-- 
--  mapL unflow :: Vector (L O) a -> Vector (L U) a    ??
--  mapL flow   :: Vector (L U) a -> Vector (L O) a

--  splitL      :: Vector r     a -> Vector (L r) a
--  joinL       :: Vector (L U) a -> Vector U a




hsplit_l 
        :: Vector Int                    -- ^ Lengths for points array
        -> Vector Point                  -- ^ Segments of points
        -> Vector (Point, Point)         -- ^ Splitting lines for each segment.
        -> IO (Vector Int, Vector Point) -- ^ Hull points found for each segment.

hsplit_l vLens vPoints vLines
 = do
        -- The determinate tells us how far away from the line each point is.
        !vDets  <- hsplit_det vLens vPoints vLines

        -- Select the points above the lines.
        !vAbove <- hsplit_above vDets vPoints

        error "sorry"


-- Compute determinate of points.
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


-- Select points above the lines.
hsplit_above
        :: Vector Double                -- ^ Determinates of points
        -> Vector Point                 -- ^ Segments of points.
        -> IO (Vector Point)            -- ^ Points that lie above the lines.

hsplit_above vDets vPoints
 = do   
        -- Select points above the lines.
        !fDets   <- F.flow vDets
        !fPoints <- F.flow vPoints
        !fAbove  <- F.pack (F.zip (F.map (> 0) fDets) fPoints)
                                -- ^ Want to use delayed arrays on the left of the zip.

        !vAbove  <- F.unflow fAbove

        return vAbove        
{-# NOINLINE hsplit_above #-}
