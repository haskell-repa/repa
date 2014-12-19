{-# LANGUAGE MagicHash, RankNTypes #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Primitive  as P
import Data.Array.Repa.Series.Ref       as Ref
import Data.Array.Repa.Series.Ref       (Ref)
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
                   _       -> error "Usage: eatPoints <size>"

        v0      <- (V.new sz :: IO (V.Vector Float))
        v1      <- V.fromPrimitive $ P.enumFromN 0 sz
        v2      <- V.fromPrimitive $ P.enumFromN 0 sz
        v0 `seq` v1 `seq` v2 `seq` return ()

        r0      <- Ref.new 0
        ((p0, x), t) 
                <- time
                $ do    True    <- runProcess2 v1 v2 (eatPoints 5 5 v0 r0)
                        p0      <- V.toPrimitive v0
                        x       <- Ref.read r0
                        p0 `seq` x `seq` return (p0, x)
        putStr $ prettyTime t
        print x


eatPoints
        :: Float -> Float
        -> R.Vector Float   -> Ref Float
        -> RateNat k
        -> R.Series k Float -> R.Series k Float
        -> Process

eatPoints ox oy v0 r0 _ s1 s2
 = let  s       = R.map2 (\x y -> (x-ox) * (x-ox) + (y-oy)* (y-oy))
                         s1 s2
   in   R.pjoin (R.reduce r0 (+) 0 s1)
                (R.fill   v0 s)
{-# NOINLINE eatPoints #-}
