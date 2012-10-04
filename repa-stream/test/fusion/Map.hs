{- Fusion tests for Map
ghc -Wall -O2 -fno-liberate-case -c Map.hs \
    -ddump-prep -dsuppress-all -dppr-case-as-let -dppr-cols200 > Map.prep
-}
{-# LANGUAGE BangPatterns #-}
module Map where
import Data.Array.Repa.Chain            as C
import Data.Vector.Unboxed              (Vector)


-- Basic map/map fusion.
fuse_map_2 :: Vector Int -> Vector Int
fuse_map_2 !vec
        = vunchain
        $ C.map (+ 1234)
        $ C.map (* 5678)
        $ vchain vec

-- map/map fusion on distributed chains.
fuse_mapD_2 :: Distro -> Vector Int -> Vector Int
fuse_mapD_2 distro !vec
        = vunchainD
        $ C.mapD (+ 1234)
        $ C.mapD (* 5678)
        $ vchainD distro vec


-- map/map fusion with many maps.
--  We shouldn't get multiple loop counters in fused code because
--  the current index is propagated from vunchainD to all the map functions.
fuse_mapD_5 :: Distro -> Vector Int -> Vector Int
fuse_mapD_5 distro !vec
        = vunchainD
        $ C.mapD (+ 1234)
        $ C.mapD (* 2345)
        $ C.mapD (* 3456)
        $ C.mapD (* 4567)
        $ C.mapD (* 5678)
        $ vchainD distro vec


-- fuse a map into the result of a replicateEach.
--    The segmented replicate operation is important for DPH, 
--    and it is based on the replicateEach function.
fuse_mapD_replicateEachD 
        :: Distro -> Distro -> Vector (Int, Int) -> Vector Int
fuse_mapD_replicateEachD !dResult !dSrc !vec
        = vunchainD
        $ C.mapD (+ 1234)
        $ C.replicateEachD dResult
        $ C.mapD (\(len, x) -> (len, x * 5678))
        $ vchainD dSrc vec

