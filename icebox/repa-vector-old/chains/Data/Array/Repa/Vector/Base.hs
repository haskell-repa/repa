
module Data.Array.Repa.Vector.Base
        ( Vector
        , balanced
        , vlength
        , vindex)
where
import Data.Array.Repa                          as R
import qualified Data.Array.Repa.Eval.Gang      as G
import qualified Data.Array.Repa.Distro         as D
import GHC.Exts

-- | Vectors are one-dimensional arrays.
type Vector r e 
        = Array r DIM1 e


-- | Get the length of a vector.
vlength :: Source r e => Vector r e -> Int
vlength !v
 = case extent v of
        Z :. len        -> len
{-# INLINE [4] vlength #-}


-- | Retrieve a single element from a vector.
vindex :: Source r e => Vector r e -> Int -> e
vindex vec ix
        = R.unsafeLinearIndex vec ix


-- | Construct a balanced `Distro` for a vector of this length,
--   dividing it evenly among the threads of the global Repa gang.
balanced :: Int -> D.Distro
balanced (I# len)
 = let  !(I# frags)     = G.gangSize G.theGang
   in   D.balanced len frags
