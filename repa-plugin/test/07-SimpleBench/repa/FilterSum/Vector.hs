{-# LANGUAGE MagicHash, RankNTypes #-}
module Main (main) where
import qualified Data.Vector.Unboxed    as U
import Data.Array.Repa.IO.Timing

import GHC.Exts
import System.Environment


---------------------------------------------------------------------
main
 = do   args <- getArgs
        let sz = case args of
                   [szStr] -> (read szStr :: Int)
                   _       -> error "Usage: simplebench <size>"
        let v1 = U.enumFromN (0 :: Int) sz
        v1 `seq` return ()
        ((v,i,a),t) <- time $ let (v,i,a) = lower_filterSum v1
                              in v `seq` i `seq` a `seq` return (v,i,a)
        putStr	$ prettyTime t
        print (U.length v, i, a)


lower_filterSum :: U.Vector Int -> (U.Vector Int, Int, Int)
lower_filterSum xs
 = let xs'  = U.filter (\x -> x > 50) xs
       sum1 = U.foldl (+) 0 xs
       sum2 = U.foldl (+) 0 xs'
   in  (xs', sum1, sum2)
{-# NOINLINE lower_filterSum #-}

