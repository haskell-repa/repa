
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
        print $ R.runSeries v1 lower_even_sum_2
        print $ R.runSeries v1 lower_maxx
        -- print $ R.runSeries v1 (lower_partial 5)     -- BROKEN


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
         (\sel ->  let  sEven       = R.pack sel s1
                        totalEven   = R.fold (+) 0 sEven
                   in   ( S.toVector sEven
                        , total
                        , totalEven))


-- | As above, but making the total syntactically inside
--   the selector context.
lower_even_sum_2 :: Series k Int -> (Vector Int, Int, Int)
lower_even_sum_2 s1
 = R.mkSel1 (R.map (\x -> x `mod` 2 == 0) s1)
   (\sel -> let  sEven  = R.pack sel s1
            in  ( S.toVector sEven
                , R.fold (+) 0 s1
                , R.fold (+) 0 sEven))


-- | Get the vector of positive values,
--   as well as the maximual element.
lower_maxx :: Series k Int -> (Vector Int, Int)
lower_maxx s1
 = R.mkSel1 (R.map (\x -> x `mod` 2 == 0) s1)
   (\sel -> let sEven   = R.pack sel s1
            in  ( S.toVector sEven
                , R.fold maxx 0 sEven))

maxx :: Int -> Int -> Int
maxx x y
 = if x > y then x else y
{-# INLINE [0] maxx #-}


-- | Get all the values more than the given limit,
--   tests out passing non-series values to fusable function.
lower_partial :: Int -> Series k Int -> Vector Int              
        -- TODO: broken, wrapper uses Int# instead of Int for first arg.
lower_partial limit s1
 = R.mkSel1 (R.map (\x -> x > limit) s1)
   (\sel -> S.toVector (R.pack sel s1))
