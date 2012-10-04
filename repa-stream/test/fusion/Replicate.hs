{- Fusion tests for Replicate.
ghc -Wall -O2 -fno-liberate-case -c Replicate.hs \
    -ddump-prep -dsuppress-all -dppr-case-as-let -dppr-cols200 > Replicate.prep
-}
{-# LANGUAGE MagicHash, BangPatterns #-}
module Replicate where
import Data.Array.Repa.Chain.Base       as C
import Data.Array.Repa.Chain.Eval       as C
import Data.Array.Repa.Chain.Replicate  as C
import Data.Vector.Unboxed              (Vector)
import qualified Data.Vector.Unboxed    as U
import GHC.Exts


fuse_replicate :: Int -> Vector Int
fuse_replicate (I# size)  
        = unchain $ C.replicate size 1234


fuse_replicateEach 
        :: Int -> Vector (Int, Int) -> Vector Int

fuse_replicateEach (I# size) !vec
 = let  !(I# len)  = U.length vec
        get ix     = vec `U.unsafeIndex` (I# ix)

   in   unchain $ C.replicateEach size (chain len get)


fuse_replicateReplicateEach
        :: Int -> Int -> Vector Int

fuse_replicateReplicateEach (I# tot) (I# n)
        = unchain 
        $ C.replicateEach tot
        $ C.replicate n (10, 1234)
