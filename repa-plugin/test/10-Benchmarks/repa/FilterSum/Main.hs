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
        ((v,i,a),t) <- time $ let (v,i,a) = R.runSeries v1 lower_filterSum
                              in v `seq` i `seq` a `seq` return (v,i,a)
        putStr	$ prettyTime t
        print (I# (V.length v), i, a)


lower_filterSum :: R.Series k Int -> (R.Vector Int, Int, Int)
lower_filterSum xs
 = let xsS = R.map (\x -> x > 50) xs
   in  R.mkSel1 xsS (\sel ->
       let xs'  = R.pack sel xs
           sum1 = R.fold (+) 0 xs
           sum2 = R.fold (+) 0 xs'
       in  (S.toVector xs', sum1, sum2))

