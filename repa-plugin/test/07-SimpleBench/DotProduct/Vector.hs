{-# LANGUAGE MagicHash, RankNTypes #-}
module Main where

import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)

import System.Environment

import Data.Array.Repa.IO.Timing

---------------------------------------------------------------------
main
 = do   args <- getArgs
        let sz = case args of
                   [szStr] -> (Prelude.read szStr :: Int)
                   _       -> error "Usage: quickhull <size>"
        let x1 = V.enumFromN 0 sz
        let y1 = V.enumFromN 0 sz
        let x2 = V.enumFromN 0 sz
        let y2 = V.enumFromN 0 sz
	x1 `seq` y1 `seq` x2 `seq` y2 `seq` return ()
        (d,t) <- time $ let d = dotp x1 y1 x2 y2    
			in  d `seq` return d
	putStr	$ prettyTime t
        print (V.head d, V.length d)

dotp :: V.Vector   Int -> V.Vector   Int
     -> V.Vector   Int -> V.Vector   Int
     -> V.Vector   Int
dotp x1 y1 x2 y2
 = (V.zipWith (+) (V.zipWith (*) x1 x2) (V.zipWith (*) y1 y2))

