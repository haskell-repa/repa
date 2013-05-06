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
 =      print $ R.streamUnboxed 
                (U.enumFromN (1 :: Int) 10) 
                lower_process


lower_process :: R.Stream k Int -> Int
lower_process s
 = R.fold (+) 0 s + R.fold (*) 1 s
{-# NOINLINE lower_process #-}



