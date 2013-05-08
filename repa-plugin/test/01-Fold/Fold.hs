module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V

import qualified Data.Vector.Unboxed    as U

-- prim binding workaround
repa_proxy_Series       :: R.Series k a -> ()
repa_proxy_Series _     = ()

repa_proxy_Vector       :: R.Vector a   -> ()
repa_proxy_Vector _     = ()

repa_addInt             = R.repa_addInt
repa_mulInt             = R.repa_mulInt

repa_newIntVector       = R.repa_newIntVector
repa_readIntVector      = R.repa_readIntVector
repa_writeIntVector     = R.repa_writeIntVector

repa_rateOfSeries       = R.repa_rateOfSeries
repa_nextInt            = R.repa_nextInt
repa_loop               = R.repa_loop


---------------------------------------------------------------------
main
 = do   v1      <- V.fromUnboxed $ U.enumFromN (1 :: Int) 10
        print $ R.runSeries v1 lower_single
        print $ R.runSeries v1 lower_process
        print $ R.runSeries v1 lower_process2
        print $ R.runSeries v1 lower_foldMap


-- Single fold.
lower_single :: R.Series k Int -> Int
lower_single s
 = R.fold (+) 0 s
{-# NOINLINE lower_single #-}


-- Double fold fusion.
--  Computation of both reductions is interleaved.
lower_process :: R.Series k Int -> Int
lower_process s
 = R.fold (+) 0 s + R.fold (*) 1 s
{-# NOINLINE lower_process #-}


-- Triple fold fusion.
--  We end up with an extra let-binding for the second baseband 
--  addition that needs to be handled properly.
lower_process2 :: R.Series k Int -> Int
lower_process2 s
 = R.fold (+) 0 s + R.fold (*) 1 s + R.fold (*) 1 s
{-# NOINLINE lower_process2 #-}


-- Fold/map fusion.
lower_foldMap :: R.Series k Int -> Int
lower_foldMap s
 = R.fold (+) 0 (R.map (\x -> x * 2) s)
{-# NOINLINE lower_foldMap #-}


{-
-- Single maps
--  The resulting code produces a vector rather than a plain Int.
lower_map :: R.Series k Int -> Vector Int
lower_map s
 = R.vectorOfSeries (R.map (\x -> x * 2 + 1) s)


-- Fold a series while mapping across it.
--  The source elements are only read from memory once.
lower_fold_map :: R.Series k Int -> (Int, Vector Int)
lower_fold_map s
 = ( R.fold (+) 0 s
   , R.map  (\x -> x * 2) s)

-}


