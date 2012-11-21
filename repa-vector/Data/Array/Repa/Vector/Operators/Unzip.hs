
module Data.Array.Repa.Vector.Operators.Unzip
        (Unzip(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Map     as R
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed      
import Prelude hiding (unzip)

class Unzip r a b where
 type TU r

 -- | Convert an array of pairs into a pair of arrays.
 unzip  :: Vector r (a, b)
        -> (Vector (TU r) a, Vector (TU r) b)


-- Unboxed --------------------------------------------------------------------
instance ( Elt a, Unbox a
         , Elt b, Unbox b) => Unzip U a b where
 type TU U = D

 unzip vec
        = (R.map fst vec, R.map snd vec)


