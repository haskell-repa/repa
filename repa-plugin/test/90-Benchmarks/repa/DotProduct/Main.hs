{-# LANGUAGE MagicHash, RankNTypes, ScopedTypeVariables, BangPatterns #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Primitive  as P
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
                   _       -> error "Usage: dotproduct <size>"

        v1 <- (V.new sz :: IO (V.Vector Float))
        x1 <- V.fromPrimitive $ P.enumFromN 1 sz
        y1 <- V.fromPrimitive $ P.enumFromN 1 sz
        x2 <- V.fromPrimitive $ P.enumFromN 1 sz
        y2 <- V.fromPrimitive $ P.enumFromN 1 sz
	x1 `seq` y1 `seq` x2 `seq` y2 `seq` return ()

        (p1,t)  <- time 
                $ do    True    <- R.runProcess4 x1 y1 x2 y2 (lower_dotp v1)
                        p1      <- V.toPrimitive v1
                        p1 `seq` return p1

        putStr	$ prettyTime t
        print (P.head p1, P.length p1)


lower_dotp 
        :: forall k
        .  R.Vector Float
        -> RateNat k
        -> R.Series k Float -> R.Series k Float
        -> R.Series k Float -> R.Series k Float
        -> Process

lower_dotp v1 _ x1 y1 x2 y2
 = R.fill v1 (R.map2 (+) (R.map2 (*) x1 x2) (R.map2 (*) y1 y2))

