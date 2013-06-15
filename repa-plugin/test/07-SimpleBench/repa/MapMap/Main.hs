{-# LANGUAGE MagicHash, RankNTypes #-}
module Main (main, repa_primitives) where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Unboxed    as U
import Data.Array.Repa.IO.Timing

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
        v1 `seq` return ()
        ((ys,zs),t) <- time $ let (ys,zs) = R.runSeries v1 lower_mapmap
                              in  ys `seq` zs `seq` return (ys,zs)
        ys'     <- V.toUnboxed ys
        zs'     <- V.toUnboxed zs
        putStr (prettyTime t)
        print (U.head ys', U.length ys', U.head zs', U.length zs')


lower_mapmap :: R.Series k Int -> (R.Vector Int, R.Vector Int)
lower_mapmap xs
 = let xs' = R.map (\x -> x * 2)  xs
       ys  = R.map (\x -> x + 50) xs'
       zs  = R.map (\x -> x - 50) xs'
   in  (S.toVector ys, S.toVector zs)
{-# NOINLINE lower_mapmap #-}


