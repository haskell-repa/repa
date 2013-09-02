{-# LANGUAGE MagicHash, BangPatterns #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Ref       as Ref
import qualified Data.Vector.Primitive  as P
import Data.Word
import GHC.Exts

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives


---------------------------------------------------------------------
main
 = do   v1      <- V.fromPrimitive $ P.enumFromN (1 :: Float) 6
        let rn  =  RateNat (V.length v1)

        r1      <- Ref.new 0
        r2      <- Ref.new 1
        R.runProcess v1 (lower_rreduce rn r1 r2)
        x1      <- Ref.read r1
        x2      <- Ref.read r2
        print (x1, x2)


-- Double reduce fusion.
--  Computation of both reductions is interleaved.
lower_rreduce 
        :: RateNat k 
        -> Ref Float -> Ref Float
        -> Series k Float -> Process
lower_rreduce _ ref1 ref2 s
 =      R.reduce ref1 (+) 0 s 
 %      R.reduce ref2 (*) 1 s
{-# NOINLINE lower_rreduce #-}
