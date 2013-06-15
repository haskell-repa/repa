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
                   _       -> error "Usage: nestedfilter <size>"
        let v1 = U.enumFromN (0 :: Int) sz
        v1 `seq` return ()
        ((v,u),t) <- time $ let (v,u) = nested v1
                              in v `seq` u `seq` return (v,u)
        putStr	$ prettyTime t
        print (U.length v, U.length u)


nested :: U.Vector Int -> (U.Vector Int, U.Vector Int)
nested xs
 = let ys  = U.filter (\x -> x > 50) xs
       zs  = U.filter (\x -> x < 100) ys
   in  (ys, zs)
{-# NOINLINE nested #-}
