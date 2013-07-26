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
        let (ys,zs) = R.runSeries v1 lower_mapmap
        print (ys,zs)

lower_mapmap :: R.Series k Int -> (R.Vector Int, R.Vector Int)
lower_mapmap xs
 = let ys = R.map (\x -> x + 50) xs
       zs = R.map (\x -> x - 50) xs
   in  (S.toVector ys, S.toVector zs)


