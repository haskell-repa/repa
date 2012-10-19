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
import Prelude                                  as P
import Data.Array.Repa.Vector.Segd              (Segd(..))
import qualified Data.Array.Repa.Vector.Segd    as Segd
import GHC.Exts
import qualified Data.Vector.Unboxed            as U

type Point      = (Double, Double)


quickHull :: Vector U Point -> IO (Vector U Point)
quickHull points
 = do   let !n          = vlength points
        
        -- Find the points that are on the extreme left and right.
        (minx, maxx)    <- minmax points
        putStrLn $ "min max       = " P.++ show (minx, maxx)

        -- Append the points together. 
        --  We'll find the hull on the top and bottom at the same time.
        !psFlat         <- R.computeUnboxedP $ R.append points points
        putStrLn $ "psFlat        = " P.++ show psFlat

        let !segd       = Segd  (R.fromListUnboxed (Z :. 2) [n, n])
                                (R.fromListUnboxed (Z :. 2) [0, n])
                                (case n + n of I# i -> i)
        putStrLn $ "segd          = " P.++ show segd

        -- Compute the hull for the top and bottom.
        -- The results from both sides are automatically concatenated.
        results         <- hsplit_l segd psFlat 
                        $  R.fromListUnboxed (R.Z R.:. (2 :: Int))
                                [ (minx, maxx), (maxx, minx) ]

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
-- hsplit points line@(p1, p2)
--  = let cross  = [: distance p line | p <- points :]
--        above = [: p | (p,c) <- zipP points cross, c D.> 0.0 :]
--    in  if lengthP packed == 0 
--          then [:p1:]
--          else let pm = points !: maxIndexP cross
--               in  [: hsplit packed ends | ends <- [:(p1, pm), (pm, p2):] :]


hsplit_l :: Segd U U                    -- Descriptor for points array.
         -> Vector U Point              -- Segments of points.
         -> Vector U (Point, Point)     -- Splitting lines for each segment.
         -> IO (Vector U Point)         -- Hull points for all segments.

hsplit_l segd points lines
 | elements segd ==# 0#
 = do   putStr  $ unlines
                [ "--- hsplit_l segd points lines --------------------------"
                , "    segd          = " P.++ show segd
                , "    points        = " P.++ show points
                , "    lines         = " P.++ show lines 
                , "" ]
        putStrLn "DONE"
        error "done"


 | otherwise
 = do   putStr  $ unlines
                [ "--- hsplit_l segd points lines --------------------------"
                , "    segd          = " P.++ show segd
                , "    points        = " P.++ show points
                , "    lines         = " P.++ show lines 
                , "" ]

        -- The dot product tells us how far from its line each point is.
        cross    <- hsplit_dot segd points lines                                        
        putStrLn $ "    cross         = " P.++ show cross

        -- The flags tell us which points are above the lines.
        flags    <- vcomputeUnboxedP $ vmap (> 0) cross
        putStrLn $ "    flags         = " P.++ show flags

        -- Select points above the lines.
        above    <- vcomputeUnboxedP $ vpack $ vzip flags points
        putStrLn $ "    above         = " P.++ show above

        -- Count how many points ended up in each segment.
        let !counts     = count_s segd flags True
        putStrLn $ "    counts        = " P.++ show counts
                 P.++ "\n"


        -- if-then-else split -------------------
        !flagsIf <- vcomputeUnboxedP
                 $  vmap (> 0) counts
        putStrLn $ "    flagsIf       = " P.++ show flagsIf


        -- if-then-else ------------------------- THEN
        !linesThen    <- vcomputeUnboxedP $ vpack  $ vzip (vmap not flagsIf) lines
        putStrLn $ "    linesThen     = " P.++ show linesThen

        !pointsHull   <- vcomputeUnboxedP $ vmap (\(p1, _) -> p1) linesThen
        putStrLn $ "    pointsHull    = " P.++ show pointsHull
        putStrLn $ ""


        -- if-then-else ------------------------- ELSE 
        !linesElse    <- vcomputeUnboxedP $ vpack  $ vzip flagsIf lines
        !crossElse    <- vcomputeUnboxedP $ vpacks flagsIf segd cross

        !countsElse   <- vcomputeUnboxedP $ vpack  $ vzip flagsIf counts

        !lengthsElse  <- vcomputeUnboxedP $ vpack  $ vzip flagsIf (Segd.lengths segd)
        let !segdElse =  Segd.fromLengths lengthsElse
        !pointsElse   <- vcomputeUnboxedP $ vpacks flagsIf segd points

        putStrLn $ "    linesElse     = " P.++ show linesElse
        putStrLn $ "    crossElse     = " P.++ show crossElse

        putStrLn $ "    segdElse      = " P.++ show segdElse
        putStrLn $ "    pointsElse    = " P.++ show pointsElse


        -- Get the points furthese from each line
        ----  The  (0, 0) below is a dummy point that will get replaced
        ----  by the first point in the segment. 
        let far (d0, p0) (d1, p1) = d1 > d0

        -- TODO: combine the zip with the pack of both crossElse and pointsElse,
        --       and fuse into select_s.
        dpoints  <- computeUnboxedP 
                 $  vzip crossElse pointsElse

        let !fars = select_s far (0, (0, 0)) segdElse dpoints
        putStrLn $ "    fars          = " P.++ show fars


        -- Append the points to each other to get the new points array.
        resultLens <- vcomputeUnboxedP
                   $  vflatten2
                   $  vmap (\c -> (c, c)) countsElse
        let !segd'         = Segd.fromLengths resultLens
        let !segdAboveElse = Segd.fromLengths countsElse

        !points'   <- vcomputeUnboxedP 
                   $  vappends segd'
                        segdAboveElse above
                        segdAboveElse above
        
        putStrLn $ "    resultLens    = " P.++ show resultLens
        putStrLn $ "    segdAboveElse = " P.++ show segdAboveElse
        putStrLn $ "    segd'         = " P.++ show segd'
        putStrLn $ "    points'       = " P.++ show points'


        -- Use the far points to make new splitting lines for the new segments.
        !lines'   <- vcomputeUnboxedP 
                  $  vflatten2 
                  $  vmap (\((p1, p2), (_, pFar)) -> ((p1, pFar), (pFar, p2)))
                  $  vzip linesElse fars
        putStrLn $ "    lines'        = " P.++ show lines'
        putStrLn $ ""

        -- Recursive call
        moar        <- if Segd.elements segd' ==# 0#
                        then return $ vfromListUnboxed []
                        else hsplit_l segd' points' lines'

        -- TODO: need to do a combine here instead of append,
        --       otherwise the points come out in the wrong order.
        results <- computeUnboxedP
                $ R.append pointsHull moar

        putStrLn $ unlines
                 [ "--- return"
                 , "   results        = " P.++ show results ]

        return results
{-# NOINLINE hsplit_l #-}


-- Take the dot product between the splitting line and each point
-- to tell us what side of the line they are on.
hsplit_dot
        :: Segd U U                     -- Descriptor for points array.
        -> Vector U Point               -- Segments of points.
        -> Vector U (Point, Point)      -- Splitting lines for each segment.
        -> IO (Vector U Double)         -- Dots for each point

-- BROKEN: the cross results are wrong with +RTS -N2
hsplit_dot !segd !points !lines 
        = vcomputeUnboxedP
        $ vzipWith
                cross_fn
                points
                (vreplicates segd lines)

 where  cross_fn (xo, yo) ((x1, y1), (x2, y2))
         = (x1 - xo) * (y2 - yo) 
         - (y1 - yo) * (x2 - xo)
        {-# INLINE cross_fn #-}
{-# NOINLINE hsplit_dot #-}


-------------------------------------------------------------------------------
-- Segmented selection.
select_s :: U.Unbox a
         => (a -> a -> Bool)
         -> a
         -> Segd U U
         -> Vector U a
         -> Vector U a

select_s choose z segd vec
 = fold_s f z segd vec
 where  f x1 x2
         = if choose x1 x2 then x2 else x1

