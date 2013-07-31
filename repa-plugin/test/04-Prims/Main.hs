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
main
 = do   v1_Int    <- V.fromUnboxed $ U.enumFromN (1 :: Int) 10
        print $ R.runSeries v1_Int    lower_ffold_Int

        v1_Float  <- V.fromUnboxed $ U.enumFromN (1 :: Float) 10
        print $ R.runSeries v1_Float  lower_ffold_Float

        v1_Double <- V.fromUnboxed $ U.enumFromN (1 :: Double) 10
        print $ R.runSeries v1_Double lower_ffold_Double


-- Int
lower_ffold_Int :: Series k Int -> Int
lower_ffold_Int s
 = R.fold (+) 0 s + R.fold (*) 1 s


-- Float
lower_ffold_Float :: Series k Float -> Float
lower_ffold_Float s
 = R.fold (+) 0 s + R.fold (*) 1 s


-- Double
lower_ffold_Double :: Series k Double -> Double
lower_ffold_Double s
 = R.fold (+) 0 s + R.fold (*) 1 s



