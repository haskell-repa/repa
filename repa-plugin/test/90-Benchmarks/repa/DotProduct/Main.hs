{-# LANGUAGE MagicHash, RankNTypes #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Unboxed    as U
import Data.Array.Repa.IO.Timing

import GHC.Exts
import System.Environment

import System.CPUTime
import System.Time
import Debug.Trace

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives

---------------------------------------------------------------------
main
 = do   args <- getArgs
        let sz = case args of
                   [szStr] -> (Prelude.read szStr :: Int)
                   _       -> error "Usage: dotprodct <size>"
        x1 <- V.fromUnboxed $ U.enumFromN 0 sz
        y1 <- V.fromUnboxed $ U.enumFromN 0 sz
        x2 <- V.fromUnboxed $ U.enumFromN 0 sz
        y2 <- V.fromUnboxed $ U.enumFromN 0 sz

	x1 `seq` y1 `seq` x2 `seq` y2 `seq` return ()
        (d,t) <- time $ let Just d  = R.runSeries4 x1 y1 x2 y2 lower_dotp
			in  d `seq` V.toUnboxed d
	putStr	$ prettyTime t
        print (U.head d, U.length d)


lower_dotp :: R.Series k Int -> R.Series k Int
     -> R.Series k Int -> R.Series k Int
     -> R.Vector   Int

lower_dotp x1 y1 x2 y2
 = S.toVector (R.map2 (+) (R.map2 (*) x1 x2) (R.map2 (*) y1 y2))

