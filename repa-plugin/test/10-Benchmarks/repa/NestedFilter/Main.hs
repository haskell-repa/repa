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
                   _       -> error "Usage: nestedfilter <size>"
        v1      <- V.fromUnboxed $ U.enumFromN (0 :: Int) sz
        v1 `seq` return ()
        ((v,u),t) <- time $ let (v,u) = R.runSeries v1 lower_nested
                              in v `seq` u `seq` return (v,u)
        putStr	$ prettyTime t
        print (I# (V.length v), I# (V.length u))

lower_nested :: R.Series k Int -> (R.Vector Int, R.Vector Int)
lower_nested xs
 = let xsS = R.map (\x -> x > 50) xs
   in  R.mkSel1 xsS (\selX ->
       let ys  = R.pack selX xs
           ysS = R.map (\x -> x < 100) ys
       in  R.mkSel1 ysS (\selY ->
           (S.toVector ys, S.toVector (R.pack selY ys))))

