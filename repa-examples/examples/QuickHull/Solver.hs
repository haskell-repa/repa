{-# LANGUAGE BangPatterns, MagicHash #-}
module Solver
        (quickHull)
where
import Data.Array.Repa                          as R
import Data.Array.Repa.Vector                   as R
import Prelude                                  as P
import Data.Array.Repa.Vector.Segd              (Segd(..))
import qualified Data.Array.Repa.Vector.Segd    as Segd
import GHC.Exts

type Point      = (Double, Double)


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
                                (case n + n of I# i -> i)
        putStrLn $ "segd    = " P.++ show segd

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

hsplit_l segd points lines
 = do   
        flags   <- hsplit_flags segd points lines                                        
        putStrLn $ "flags =  " P.++ show flags

        packed  <- vcomputeUnboxedP $ vpack $ vzip flags points
        putStrLn $ "packed = " P.++ show packed

        -- TODO: need count_s to work out how many elements went into each segment.

        return $ error "hsplit_l: finish me"


-- Take the dot product between the splitting line and each point
-- to tell us what side of the line they are on.
--
-- TODO: try to fuse the packs and counts into this.
--       Twin streams? Can we produce the packed points and new segment lengths
--       during the same computation?
--
--      unstreamTwin :: Stream (a, Maybe b) -> (Vector a, Vector b)
-- 
--      unstreamFold :: (a -> a -> a) -> a -> Stream a -> (Vector a, a)
-- 
hsplit_flags
        :: Segd U U                     -- Descriptor for points array.
        -> Vector U Point               -- Segments of points.
        -> Vector U (Point, Point)      -- Splitting lines for each segment.
        -> IO (Vector U Bool)           -- Flags for each value.

-- BROKEN: the cross results are wrong with +RTS -N2
hsplit_flags !segd !points !lines 
        = vcomputeUnboxedP
        $ vmap (> 0)
        $ vzipWith
                cross_fn
                points
                (vreplicates segd lines)

 where  cross_fn (xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) 
         - (y1 - yo) * (x2 - xo)
        {-# INLINE cross_fn #-}

{-# NOINLINE hsplit_flags #-}
