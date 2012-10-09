
module Data.Array.Repa.Vector.Base
        ( Vector
        , vlength)
where
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U


-- | Vectors are one-dimensional arrays.
type Vector r e 
        = Array r DIM1 e


-- | Get the length of a vector.
vlength :: Source r e => Vector r e -> Int
vlength !v
 = case extent v of
        Z :. len        -> len
{-# INLINE [4] vlength #-}

