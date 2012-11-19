
module Data.Array.Repa.Vector.Base
        ( Vector
        , Zip(..)
        , vlength)
where
import Data.Array.Repa                          as R
import Prelude                                  hiding (length)
import Prelude hiding (zip, map, length, replicate)


-- | A Repa vector is just an alias for a 1D array.
type Vector r e 
        = Array r DIM1 e


class Zip r1 r2 a b where
 type TZ r1 r2
 -- | Vector zip uses the least general possible representation for the result.
 zip    :: Vector r1 a 
        -> Vector r2 b
        -> Vector (TZ r1 r2) (a, b)



-------------------------------------------------------------------------------
-- | Get the length of a vector.
vlength :: Source r e => Vector r e -> Int
vlength !v
 = case extent v of
        Z :. len        -> len
{-# INLINE [4] vlength #-}
