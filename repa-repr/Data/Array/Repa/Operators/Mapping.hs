
module Data.Array.Repa.Operators.Mapping
        (map)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Delayed
import Prelude hiding (map)

map     :: (Shape sh, Repr r a)
        => (a -> b) -> Array r sh a -> Array D sh b

map f arr
 = case load arr of
        Delayed sh g    -> Delayed sh (f . g)