
module Data.Array.Repa.Vector.Base
        ( Array
        , Vector
        , Elt(..)
        , module Data.Array.Repa.Vector.Index
        , module Data.Array.Repa.Vector.Shape)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Vector.Index
import Data.Array.Repa.Vector.Shape


-- | Arrays with a representation type, shape and element type.
data family Array r sh e


-- | A Repa vector is just an alias for a 1D array.
type Vector r e 
        = Array r DIM1 e

