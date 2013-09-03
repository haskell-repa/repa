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

        -- rreduce
        r1      <- Ref.new 0
        r2      <- Ref.new 1
        R.runProcess v1 (rreduce r1 r2)
        x1      <- Ref.read r1
        x2      <- Ref.read r2
        print (x1, x2)

        -- rreduce2
        r1      <- Ref.new 0
        r2      <- Ref.new 1
        R.runProcess v1 (rreduce2 r1 r2)
        x1      <- Ref.read r1
        x2      <- Ref.read r2
        print (x1, x2)

        -- 


-- Double reduce fusion.
--  Computation of both reductions is interleaved.
rreduce :: Ref Float -> Ref Float
        -> RateNat k -> Series k Float 
        -> Process
rreduce ref1 ref2 _ s
 =      R.reduce ref1 (+) 0 s 
 %      R.reduce ref2 (*) 1 s
{-# NOINLINE rreduce #-}


-- Double reduce fusion.
--  Computation of both reductions is interleaved.
rreduce2
        :: Ref Float -> Ref Float
        -> RateNat k -> Series k Float 
        -> Process
rreduce2 ref1 ref2 _ s0
 = let  s1      = R.map  (+ 1) s0
        s2      = R.map  (* 5) s0
        s3      = R.map2 (+) s1 s2
   in   R.reduce ref1 (+) 0 s2
    %   R.reduce ref2 (+) 0 s3
{-# NOINLINE rreduce2 #-}
