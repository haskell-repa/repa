
module Data.Array.Repa.Vector.Base
        ( Vector
        , length)
where
import Data.Array.Repa          as R
import Prelude                  hiding (length)


-- | A Repa vector is just an alias for a 1D array.
type Vector r e 
        = Array r DIM1 e


-- | Get the length of a vector.
length :: Source r e => Vector r e -> Int
length !v
 = case extent v of
        Z :. len        -> len
{-# INLINE [4] length #-}
