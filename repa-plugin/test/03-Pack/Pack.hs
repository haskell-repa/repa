
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
 = do   v1      <- V.fromUnboxed $ U.enumFromN (1 :: Int) 10
       
        print $ R.runSeries v1 lower_even
        print $ R.runSeries v1 lower_even_sum
--        print $ R.runSeries v1 lower_even_sum_2


-- | Return just the even values.
lower_even :: Series k Int -> Vector Int
lower_even s1
 = R.mkSel1 
        (R.map (\x -> x `mod` 2 == 0) s1)
        (\sel -> S.toVector (R.pack sel s1))


-- | Return the positive values, 
--   along with the sum of all, and sum of just positive.
lower_even_sum :: Series k Int -> (Vector Int, Int, Int)
lower_even_sum s1
 = let  total   = R.fold (+) 0 s1
        flags   = R.map (\x -> x `mod` 2 == 0) s1
   in   R.mkSel1 flags
         (\sel ->  let  sPositive       = R.pack sel s1
                        totalPositive   = R.fold (+) 0 sPositive
                   in   ( S.toVector sPositive
                        , total
                        , totalPositive))


-- | As above, but making the total syntactically inside
--   the selector context.
lower_even_sum_2 :: Series k Int -> (Vector Int, Int, Int)
lower_even_sum_2 s1
 = R.mkSel1 (R.map (\x -> x `mod` 2 == 0) s1)
   (\sel -> let  sPositive       = R.pack sel s1
            in  ( S.toVector sPositive
                , R.fold (+) 0 s1
                , R.fold (+) 0 sPositive))
