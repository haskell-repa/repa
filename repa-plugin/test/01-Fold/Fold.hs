module Main where
import Data.Array.Repa.Series           as R
import qualified Data.Vector.Unboxed    as U

-- prim binding workaround
repa_addInt             = R.repa_addInt
repa_mulInt             = R.repa_mulInt

repa_newByteArray       = R.repa_newByteArray
repa_readIntArray       = R.repa_readIntArray
repa_writeIntArray      = R.repa_writeIntArray

repa_rateOfStream       = R.repa_rateOfStream
repa_nextInt            = R.repa_nextInt
repa_loop               = R.repa_loop


---------------------------------------------------------------------
main
 = do   let s1  = U.enumFromN (1 :: Int) 10

        print $ R.streamUnboxed s1 lower_single
        print $ R.streamUnboxed s1 lower_process
        print $ R.streamUnboxed s1 lower_process2
        print $ R.streamUnboxed s1 lower_foldMap


-- Single fold.
-- TODO: not lowered.
lower_single :: R.Stream k Int -> Int
lower_single s
 = R.fold (+) 0 s
{-# NOINLINE lower_single #-}


-- Double fold fusion.
lower_process :: R.Stream k Int -> Int
lower_process s
 = R.fold (+) 0 s + R.fold (*) 1 s
{-# NOINLINE lower_process #-}


-- Triple fold fusion.
--  We end up with an extra let-binding for the second base-band 
--  addition that needs to be handled properly.
lower_process2 :: R.Stream k Int -> Int
lower_process2 s
 = R.fold (+) 0 s + R.fold (*) 1 s + R.fold (*) 1 s
{-# NOINLINE lower_process2 #-}


-- Fold/map fusion.
lower_foldMap :: R.Stream k Int -> Int
lower_foldMap s
 = R.fold (+) 0 (R.map (\x -> x * 2) s)
{-# NOINLINE lower_foldMap #-}
