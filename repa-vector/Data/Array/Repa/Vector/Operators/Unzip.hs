
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
 unzip  :: Shape sh
        => Array r sh (a, b)
        -> (Array (TU r) sh a, Array (TU r) sh b)


-- Unboxed --------------------------------------------------------------------
instance ( Elt a, Unbox a
         , Elt b, Unbox b) => Unzip U a b where
 type TU U = D

 unzip vec
        = (R.map fst vec, R.map snd vec)


