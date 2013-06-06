{-# LANGUAGE MagicHash, RankNTypes #-}
module Main where
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
        v1      <- V.fromUnboxed $ U.enumFromN (0 :: Int) sz `U.zip` U.enumFromN (0 :: Int) sz
        let (ys,zs) = R.runSeries v1 (lower_points 5 5)
        print (ys,zs)

lower_points :: Int -> Int
             -> R.Series k (Int, Int)
             -> (R.Vector Int, Int)
lower_points ox oy xs
 = let dists = R.map  (\(x,y) -> (x-ox)*(x-ox) + (y-oy)*(y-oy)) xs
       mini  = R.fold min 1000 dists
   in  (S.toVector dists, mini)




