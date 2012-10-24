--
-- TODO: When this is working make a version that does each operation
--       individually, with debugging. And try to fuse everything
--       in a separate version.

-- TODO: Should be able to compute cross/flags/above/counts in the same
--       computation. Twin streams? Can we produce the packed points and
--       new segment lengths during the same computation?
--
--       unstreamTwin :: Stream (a, Maybe b) -> (Vector a, Vector b)
--       unstreamFold :: (a -> a -> a) -> a -> Stream a -> (Vector a, a)
-- 
-- TODO: Make a combinator that produces both sides of a if-then-else split
--       with the same pass over the source array.
--
--       unstream2 :: Stream (Bool, a, b) -> (Vector a, Vector b)
--
--       If both results need to be consumed more than once then we can 
--       make them both manifest in the same computation. If each result
--       only needs to be consumed once we sholuld fuse the pack into the 
--       consumer.
--
-- TODO: When doing packing etc, should pack point indices
--       instead of packing the physical points, though this would be 
--       a change to the original DPH example.
--
{-# LANGUAGE BangPatterns, MagicHash #-}
module Solver
        (quickHull)
where
import Data.Array.Repa                          as R
import Data.Array.Repa.Vector                   as R
import Data.Array.Repa.Vector.Segd              (Segd(..))
import qualified Data.Array.Repa.Vector.Segd    as Segd
import qualified Data.Array.Repa.Vector.Sel     as Sel2
import qualified Data.Vector.Unboxed            as U
import Prelude                                  as P
import GHC.Exts


-- | A point in the 2D plane.
type Point      = (Double, Double)


-- | Compute the convex hull of a vector of points.
quickHull :: Vector U Point -> IO (Vector U Point)
quickHull points
 = do
        -- Find the points that are on the extreme left and right.
        (minx, maxx)    <- minmax points

        -- Append the points together. 
        --  We'll find the hull on the top and bottom at the same time.
        let !n          =  vlength points
        let !downSegd   =  Segd.fromLengths $ vfromUnboxed $ U.fromList [n, n]
        !downPoints     <- R.computeUnboxedP $ R.append points points

        putStrLn $ "min max       = " P.++ show (minx, maxx)
        putStrLn $ "downSegd      = " P.++ show downSegd
        putStrLn $ "downPoints    = " P.++ show downPoints

        -- Use the min/max points in both directions to get the 
        -- hull on the top and bottom halves of the plane.
        let !downLines  = vfromListUnboxed [ (minx, maxx), (maxx, minx) ]
        putStrLn $ "downLines     = " P.++ show downLines

        -- Compute the hull for the top and bottom.
        -- The results from both sides are automatically concatenated.
        (_, results)    <- hsplit_l downSegd downPoints downLines

        return results


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


-- hsplit_l -------------------------------------------------------------------
--
-- | Lifted split.
--   This is the workhorse of the algorithm.
--
--   Unvectorised code:
--
--   > hsplit points line@(p1, p2)
--   >  = let cross  = [: distance p line | p <- points :]
--   >        above  = [: p | (p,c) <- zipP points cross, c D.> 0.0 :]
--   >    in  if lengthP packed == 0 
--   >          then [:p1:]
--   >          else let pm = points !: maxIndexP cross
--   >               in  concat [: hsplit packed ends 
--   >                          | ends <- [:(p1, pm), (pm, p2):] :]
--
hsplit_l :: Segd U U                      -- Segd for points array.
         -> Vector U Point                -- Segments of points.
         -> Vector U (Point, Point)       -- Splitting lines for each segment.
         -> IO (Segd U U, Vector U Point) -- Hull points found for each segment.

hsplit_l segd points lines
 -- No points to process, we're done already.
 | elements segd ==# 0#
 = do   let !segd'      = Segd.fromLengths 
                        $ vfromUnboxed
                        $ U.replicate (I# (Segd.elements segd)) 0

        let !points'    = vfromListUnboxed []
        return (segd', points')

 | otherwise
 = do   putStr  $ unlines
                [ "--- DIVIDE ---------------------------------------"
                , "    segd          = " P.++ show segd
                , "    points        = " P.++ show points
                , "    lines         = " P.++ show lines 
                , "" ]

        -- The determinate product tells us how far from its line each point is.
        det     <- hsplit_det segd points lines                                        
        putStrLn $ "    det           = " P.++ show det

        -- The flags tell us which points are above the lines.
        -- TODO: recompute flags from crosses both times instead of
        --       materializing the flags vector.
        let !flags = vcomputeUnboxedP $ vmap (> 0) det
        putStrLn $ "    flags         = " P.++ show flags

        -- Select points above the lines.
        let !above = vcomputeUnboxedP $ vpack $ vzip flags points
        putStrLn $ "    above         = " P.++ show above

        -- Count how many points ended up in each segment.
        let !counts     = count_s (Segd.splitSegd segd) flags True
        putStrLn $ "    counts        = " P.++ show counts
                 P.++ "\n"


        -- if-then-else split -------------------
        -- TODO: recompute flags from counts both time instead of
        --       materializing the flags vector.
        let !flagsIf    = vcomputeUnboxedP $ vmap (> 0) counts
        putStrLn $ "    flagsIf       = " P.++ show flagsIf


        -- if-then-else ------------------------- THEN
        let !linesThen  = vcomputeUnboxedP $ vpack $ vzip (vmap not flagsIf) lines
        putStrLn $ ""
        putStrLn $ "    linesThen     = " P.++ show linesThen

        let !hullSegd   = Segd.fromLengths $ vfromUnboxed $ U.replicate (vlength linesThen) 1
        let !hullPoints = vcomputeUnboxedP $ vmap (\(p1, _) -> p1) linesThen
        putStrLn $ "    hullSegd      = " P.++ show hullSegd
        putStrLn $ "    hullPoints    = " P.++ show hullPoints
        putStrLn $ ""


        -- if-then-else ------------------------- ELSE 
        let !linesElse   = vcomputeUnboxedP $ vpack  $ vzip flagsIf lines
        let !detElse     = vcomputeUnboxedP $ vpacks flagsIf segd det

        let !countsElse  = vcomputeUnboxedP $ vpack  $ vzip flagsIf counts

        let !lengthsElse = vcomputeUnboxedP $ vpack  $ vzip flagsIf (Segd.lengths segd)
        let !segdElse    = Segd.fromLengths lengthsElse
        let !pointsElse  = vcomputeUnboxedP $ vpacks flagsIf segd points

        putStrLn $ "    linesElse     = " P.++ show linesElse
        putStrLn $ "    detElse       = " P.++ show detElse

        putStrLn $ "    segdElse      = " P.++ show segdElse
        putStrLn $ "    pointsElse    = " P.++ show pointsElse


        -- Get the points furthese from each line
        ----  The  (0, 0) below is a dummy point that will get replaced
        ----  by the first point in the segment. 
        let far (d0, p0) (d1, p1) = d1 > d0

        -- TODO: combine the zip with the pack of both crossElse and pointsElse,
        --       and fuse into select_s.
        let !dpoints    = vcomputeUnboxedP $ vzip detElse pointsElse

        let !fars       = vselects far (0, (0, 0)) segdElse dpoints
        putStrLn $ "    fars          = " P.++ show fars


        -- Append the points to each other to get the new points array.
        let !downLens   = vcomputeUnboxedP
                        $ vflatten2
                        $ vmap (\c -> (c, c)) countsElse

        let !downSegd2     = Segd.fromLengths (vfromUnboxed $ U.replicate (vlength countsElse) 2)
        let !downSegd      = Segd.fromLengths downLens
        let !segdAboveElse = Segd.fromLengths countsElse

        let !downPoints = vcomputeUnboxedP 
                        $ vappends downSegd
                                segdAboveElse above
                                segdAboveElse above
        
        putStrLn $ "    segdAboveElse = " P.++ show segdAboveElse
        putStrLn $ ""
        putStrLn $ "    downSegd2     = " P.++ show downSegd2
        putStrLn $ "    downSegd      = " P.++ show downSegd
        putStrLn $ "    downPoints    = " P.++ show downPoints


        -- Use the far points to make new splitting lines for the new segments.
        let !downLines  = vcomputeUnboxedP 
                        $ vflatten2 
                        $ vmap (\((p1, p2), (_, pFar)) -> ((p1, pFar), (pFar, p2)))
                        $ vzip linesElse fars

        putStrLn $ "    downLines     = " P.++ show downLines
        putStrLn $ ""

        -- Recursive call
        (moarSegd, moarPoints) 
                <- hsplit_l downSegd downPoints downLines


        -- Concatenate the segments we get from the recursive call.
        --   In the recursion we pass down two segments for each one that we
        --   had from above.
        let !catLengths    = sum_s (Segd.splitSegd downSegd2) (Segd.lengths moarSegd)
        let !catSegd       = Segd.fromLengths catLengths


        -- Combine the lengths of the segments we get from both sides of the
        -- if-then-else.
        let !combLengths   = vcombine2 flagsIf 
                                (Segd.lengths hullSegd)
                                (Segd.lengths catSegd)

        let !combSegd      = Segd.fromLengths combLengths


        -- Combine the points from both sides of the if-then-else.
        let !combPoints    = vcombineSegs2 flagsIf 
                                (Segd.lengths hullSegd) hullPoints 
                                (Segd.lengths catSegd)  moarPoints

        putStrLn $ unlines
                 [ "--- RETURN --------------------------------------"
                 , "    segd          = " P.++ show segd
                 , ""
                 , "    flagsIf       = " P.++ show flagsIf
                 , "    segdElse      = " P.++ show segdElse
                 , ""
                 , "    hullPoints    = " P.++ show hullPoints
                 , "    hullSegd      = " P.++ show hullSegd
                 , ""
                 , "    downSegd2     = " P.++ show downSegd2
                 , "    downSegd      = " P.++ show downSegd
                 , "    moarPoints    = " P.++ show moarPoints
                 , "    moarSegd      = " P.++ show moarSegd
                 , ""
                 , "    catSegd       = " P.++ show catSegd
                 , ""
                 , "    combLengths   = " P.++ show combLengths
                 , "    combPoints    = " P.++ show combPoints ]

        return (combSegd, combPoints)
{-# NOINLINE hsplit_l #-}


-- Take the determinate between the splitting line and each point
-- to tell us what side of the line they are on.
--
-- BROKEN: the results here are wrong with +RTS -N2
hsplit_det
        :: Segd U U                     -- Descriptor for points array.
        -> Vector U Point               -- Segments of points.
        -> Vector U (Point, Point)      -- Splitting lines for each segment.
        -> IO (Vector U Double)         -- Determinates for each point

hsplit_det !segd !points !lines 
        = return
        $ vcomputeUnboxedP
        $ vzipWith
                det_fn
                points
                (vreplicates segd lines)

 where  det_fn (xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) 
         - (y1 - yo) * (x2 - xo)
        {-# INLINE det_fn #-}
{-# NOINLINE hsplit_det #-}


-------------------------------------------------------------------------------
-- Segmented selection.
vselects :: U.Unbox a
         => (a -> a -> Bool)
         -> a
         -> Segd U U
         -> Vector U a
         -> Vector U a

vselects choose z segd vec
 = fold_s f z (Segd.splitSegd segd) vec
 where  f x1 x2
         = if choose x1 x2 then x2 else x1

