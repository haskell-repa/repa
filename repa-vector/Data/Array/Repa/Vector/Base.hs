
module Data.Array.Repa.Vector.Base
        ( Array
        , Vector
        , module Data.Array.Repa.Vector.Index
        , module Data.Array.Repa.Vector.Shape
        , module Data.Array.Repa.Vector.Elt)
where
import Data.Array.Repa.Vector.Index
import Data.Array.Repa.Vector.Shape
import Data.Array.Repa.Vector.Elt


-- | Arrays with a representation type, shape and element type.
data family Array r sh e


-- | A Repa vector is just an alias for a 1D array.
type Vector r e 
        = Array r DIM1 e

