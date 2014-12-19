{-# LANGUAGE MagicHash, RankNTypes #-}
module Main (main, repa_primitives) where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Primitive  as P
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
                   _       -> error "Usage: nestedFilter <size>"

        v0      <- V.fromPrimitive $ P.enumFromN (0 :: Int) sz
        v1      <- V.new sz
        v2      <- V.new sz

        v0 `seq` v1 `seq` v2 `seq` return ()
        ((),t)  <- time $  R.runProcess v0 (nestedFilter v1 v2)

        p1      <- V.toPrimitive v1
        p2      <- V.toPrimitive v2

        putStr	$ prettyTime t
        print (P.length p1, P.length p2)


nestedFilter 
        :: R.Vector Int -> R.Vector Int
        -> R.RateNat k  -> R.Series k Int
        -> Process

nestedFilter v1 v2 _ s0
 = let fs1      = R.map (\x -> x > 50) s0
   in  R.mkSel1 fs1 (\sel1 ->
        let s1   = R.pack sel1 s0
            fs2  = R.map (\x -> x < 100) s1
        in R.mkSel1 fs2 (\sel2 ->
            let s2      = R.pack sel2 s1
            in  R.fill v1 s1
             %  R.fill v2 s2))
{-# NOINLINE nestedFilter #-}
