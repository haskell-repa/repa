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
        -- We'll find the hull on the top and bottom at the same time.
        --  TODO: shouldn't need to do this, just split the points.
        !psFlat         <- R.computeUnboxedP $ R.append points points
        putStrLn $ "psFlat        = " P.++ show psFlat

        let !segd       = Segd  (R.fromListUnboxed (Z :. 2) [n, n])
                                (R.fromListUnboxed (Z :. 2) [0, n])
                                (case n + n of I# i -> i)
        putStrLn $ "segd          = " P.++ show segd

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

-- flatten2 
-- TODO: Implement these as a family of chain operations.
-- flatten2 :: Vector U (a, a) -> Vector D a

-- flatten2 vec
--  = vmap get $ vindexed vec
--  where  get (ix, (a, b))
--                 = V.map 


-- | TODO: When doing this packing etc, should pack point indices
--         instead of packing the physical points.
--
hsplit_l :: Segd U U                    -- Descriptor for points array.
         -> Vector U Point              -- Segments of points.
         -> Vector U (Point, Point)     -- Splitting lines for each segment.
         -> IO (Vector U Point)         -- Hull points for all segments.

hsplit_l segd points lines
 | elements segd ==# 0#
 = error "done"

 | otherwise
 = do   putStr  $ unlines
                [ "--- hsplit_l segd points lines --------------------------"
                , "    segd          = " P.++ show segd
                , "    points        = " P.++ show points
                , "    lines         = " P.++ show lines 
                , "" ]

        -- TODO: check for zero length segments and add first point of lines
        --       as the new hull points.

        -- The dot product tells us how far from its line each point is.
        dot      <- hsplit_dot segd points lines                                        
        putStrLn $ "    dot           = " P.++ show dot

        -- Get the points furthest from each line.
        --  The  (0, 0) below is a dummy point that will get replaced
        --  by the first point in the segment. 
        --  TODO: check for zero segment lengths beforehand.
        let far (d0, p0) (d1, p1) = d1 > d0
        dpoints  <- computeUnboxedP $ vzip dot points
        let !fars = select_s far (0, (0, 0)) segd dpoints
        putStrLn $ "    fars          = " P.++ show fars

        -- The flags tell us which points are above the lines.
        -- TODO: dots is being consumed by the 'far' check as well as 'flags' check
        --       it won't fuse into them.
        flags    <- vcomputeUnboxedP $ vmap (> 0) dot
        putStrLn $ "    flags         = " P.++ show flags

        -- Select points above the lines.
        packed   <- vcomputeUnboxedP $ vpack $ vzip flags points
        putStrLn $ "    packed        = " P.++ show packed

        -- Count how many points ended up in each segment.
        let !counts     = count_s segd flags True
        let !packedSegd = Segd.fromLengths counts
        putStrLn $ "    counts        = " P.++ show counts


        -- Append the points to each other to get the new points array.
        let newCount c = (c, c)
        resultLens <- vcomputeUnboxedP
                   $  vflatten2
                   $  vmap newCount counts
        putStrLn $ "    counts'       = " P.++ show resultLens

        let !segd' = Segd.fromLengths resultLens
        putStrLn $ "    segd'         = " P.++ show segd'

        !points'   <- vcomputeUnboxedP 
                   $  vappends segd'
                        packedSegd packed
                        packedSegd packed

        putStrLn $ "    points'       = " P.++ show points'


        -- Use the far points to make new splitting lines for the new segments.
        let newLines ((p1, p2), (_, pFar))
                = ((p1, pFar), (pFar, p2))

        !lines' <- vcomputeUnboxedP 
                $  vflatten2 
                $  vmap newLines 
                $  vzip lines fars
        putStrLn $ "    lines'        = " P.++ show lines'
        putStrLn $ ""

        -- Recursive call
        hsplit_l segd' points' lines'


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
