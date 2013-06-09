{-# LANGUAGE MagicHash, RankNTypes #-}
module Main (main, repa_primitives) where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Unboxed    as U

import GHC.Exts
import System.Environment

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives

---------------------------------------------------------------------
main
 = do   args <- getArgs
        let sz = case args of
                   [szStr] -> (Prelude.read szStr :: Int)
                   _       -> error "Usage: simplebench <size>"
        v1      <- V.fromUnboxed $ U.enumFromN (0 :: Int) sz
        let (v,i,a) = R.runSeries v1 lower_filterSum
        print (I# (V.length v), i, a)

lower_filterSum :: R.Series k Int -> (R.Vector Int, Int, Int)
lower_filterSum xs
 = let xsS = R.map (\x -> x > 50) xs
   in  R.mkSel1 xsS (\sel ->
       let xs'  = R.pack sel xs
           sum1 = R.fold (+) 0 xs
           sum2 = R.fold (+) 0 xs'
       in  (S.toVector xs', sum1, sum2))

{-
lower_grades :: R.Series k Int -> (Int,Int,Int)
lower_grades xs
 = let p0  = R.map (\x -> x >= pass && x < cred) xs
       p0' = R.mkSel1 p0
                (\sel ->
                    let x' = R.pack sel xs
                    in  R.fold (+) 0 x' `div` I# (S.length x'))
       p1  = R.map (\x -> x >= pass && x < cred) xs
       p1' = R.mkSel1 p1
                (\sel ->
                    let x' = R.pack sel xs
                    in  R.fold (+) 0 x' `div` I# (S.length x'))
       p2  = R.map (\x -> x >= cred && x < d) xs
       p2' = R.mkSel1 p2
                (\sel ->
                    let x' = R.pack sel xs
                    in  R.fold (+) 0 x' `div` I# (S.length x'))
   in  (p0', p1', p2')

pass :: Int
pass = 50
cred :: Int
cred = 70
d    :: Int
d    = 80
hd    :: Int
hd   = 90
-}
