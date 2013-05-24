module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Unboxed    as U

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives

---------------------------------------------------------------------
-- | The opaque worker function for map
-- Noinline, because we want to make sure lower can handle arbitrary functions
{-# NOINLINE f_opaque #-}
f_opaque :: Int -> Int
f_opaque x = x + 1

---------------------------------------------------------------------
main
 = do   v1      <- V.fromUnboxed $ U.enumFromN (1 :: Int) 10
        print $ R.runSeries v1 lower_map


-- Fold a series while mapping across it.
--  The source elements are only read from memory once.
lower_map :: R.Series k Int -> (Vector Int)
lower_map s
 = S.toVector (R.map f_opaque s)

