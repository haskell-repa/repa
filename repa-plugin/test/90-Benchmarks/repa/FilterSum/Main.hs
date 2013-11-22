{-# LANGUAGE MagicHash, RankNTypes #-}
module Main (main, repa_primitives) where
import Data.Array.Repa.IO.Timing
import Data.Array.Repa.Series                   as R
import Data.Array.Repa.Series.Series            as S
import Data.Array.Repa.Series.Vector            as V
import Data.Array.Repa.Series.Ref               (Ref)
import qualified Data.Array.Repa.Series.Ref     as Ref
import qualified Data.Vector.Primitive          as P
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
                   _       -> error "Usage: filterSum <size>"

        v1      <- V.fromPrimitive $ P.enumFromN (0 :: Int) sz
        v2      <- V.new sz
        r1      <- Ref.new 0
        r2      <- Ref.new 0

        v1 `seq` return ()
        v2 `seq` return ()
        ((),t)  <- time $ R.runProcess v1 (filterSum v2 r1 r2) 

        p2      <- V.toPrimitive v2
        x1      <- Ref.read r1
        x2      <- Ref.read r2

        putStr	$ prettyTime t
        print (P.length p2, x1, x2)


filterSum 
        :: Vector Int -> Ref Int -> Ref Int
        -> RateNat k -> R.Series k Int 
        -> Process

filterSum v2 r1 r2 _ s1
 = let s2 = R.map (\x -> x > 50) s1
   in  R.mkSel1 s2 (\sel ->
        let s3   = R.pack sel s1
        in  R.fill v2 s3
         %  R.reduce r1 (+) 0 s1
         %  R.reduce r2 (+) 0 s3)
{-# NOINLINE filterSum #-}
