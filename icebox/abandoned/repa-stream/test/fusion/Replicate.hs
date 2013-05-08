{- Fusion tests for Replicate.
ghc -Wall -O2 -fno-liberate-case -c Replicate.hs \
    -ddump-prep -dsuppress-all -dppr-case-as-let -dppr-cols200 > Replicate.prep
-}
{-# LANGUAGE MagicHash, BangPatterns #-}
module Replicate where
import Data.Array.Repa.Chain            as C
import Data.Vector.Unboxed              (Vector)
import GHC.Exts

-- replicate ------------------------------------------------------------------
fuse_replicate :: Int -> Vector Int
fuse_replicate (I# size)  
        = vunchain $ C.replicate size 1234


fuse_replicateD :: Distro -> Vector Int
fuse_replicateD distro
        = vunchainD $ C.replicateD distro 1234


-- replicateEach --------------------------------------------------------------
fuse_replicateEach 
        :: Int -> Vector (Int, Int) -> Vector Int
fuse_replicateEach (I# size) !vec
        = vunchain $ C.replicateEach size $ vchain vec


fuse_replicateEachD 
        :: Distro -> Distro -> Vector (Int, Int) -> Vector Int
fuse_replicateEachD dResult dSrc !vec
        = vunchainD $ C.replicateEachD dResult $ vchainD dSrc vec


fuse_replicateReplicateEach
        :: Int -> Int -> Vector Int
fuse_replicateReplicateEach (I# tot) (I# n)
        = vunchain 
        $ C.replicateEach tot
        $ C.replicate n (10, 1234)


