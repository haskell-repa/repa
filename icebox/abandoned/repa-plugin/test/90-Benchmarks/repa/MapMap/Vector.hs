{-# LANGUAGE MagicHash, RankNTypes, BangPatterns #-}
module Main (main) where
import qualified Data.Vector.Unboxed    as U
import Data.Array.Repa.IO.Timing

import GHC.Exts
import System.Environment

---------------------------------------------------------------------
main
 = do   args <- getArgs
        let sz = case args of
                   [szStr] -> (Prelude.read szStr :: Int)
                   _       -> error "Usage: simplebench <size>"
        let !v1      = U.enumFromN (0 :: Int) sz
        v1 `seq` return ()
        ((ys,zs),t) <- time $ let (ys,zs) = lower_mapmap v1
                              in  ys `seq` zs `seq` return (ys,zs)
        putStr (prettyTime t)
        print (U.head ys, U.length ys, U.head zs, U.length zs)


lower_mapmap :: U.Vector Int -> (U.Vector Int, U.Vector Int)
lower_mapmap !xs
 = let !xs' = U.map (\x -> x * 2)  xs
       !ys  = U.map (\x -> x + 50) xs'
       !zs  = U.map (\x -> x - 50) xs'
   in  (ys, zs)
{-# NOINLINE lower_mapmap #-}
