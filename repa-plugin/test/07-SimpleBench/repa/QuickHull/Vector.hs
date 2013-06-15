{-# LANGUAGE MagicHash, RankNTypes #-}
module Main where

import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)
import Data.Array.Repa.IO.Timing

import System.Environment

---------------------------------------------------------------------
main
 = do   args <- getArgs
        let sz = case args of
                   [szStr] -> (Prelude.read szStr :: Int)
                   _       -> error "Usage: quickhull <size>"
        let pts  = gen 23489 sz `V.zip` gen 12387 sz
        pts `seq` return ()
        (pts',t) <- time $ let p' = quickHull pts
                           in  p' `seq` return p'
        putStr	$ prettyTime t
        print pts'


gen :: Int -> Int -> Vector Int
gen seed size
 = V.generate size r
 where
  r i = i * (seed*5319) `mod` (seed * 978) `mod` 500


quickHull :: Vector (Int, Int) -> Vector (Int, Int)
quickHull vv
	= uncurry V.zip $ quickhull $ V.unzip vv


quickhull :: (Vector Int, Vector Int) -> (Vector Int, Vector Int)
quickhull (xs, ys) = xs' `seq` ys' `seq` (xs',ys')
    where
      (xs',ys') = V.unzip
                $ hsplit points pmin pmax V.++ hsplit points pmax pmin

      imin = V.minIndex xs
      imax = V.maxIndex xs

      points = V.zip xs ys
      pmin   = points V.! imin
      pmax   = points V.! imax


      hsplit points p1 p2
        | V.length packed < 2 = p1 `V.cons` packed
        | otherwise = hsplit packed p1 pm V.++ hsplit packed pm p2
        where
          cs     = V.map (\p -> cross p p1 p2) points
          packed = V.map fst
                 $ V.filter (\t -> snd t > 0)
                 $ V.zip points cs

          pm     = points V.! V.maxIndex cs

      cross (x,y) (x1,y1) (x2,y2) 
        = (x1-x)*(y2-y) - (y1-y)*(x2-x)
{-# NOINLINE quickhull #-}

