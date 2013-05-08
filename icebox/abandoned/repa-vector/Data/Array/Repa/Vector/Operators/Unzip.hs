
module Data.Array.Repa.Vector.Operators.Unzip
        ( Unzip(..)
        , unzip3)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Map     as R
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed      
import Prelude hiding (unzip, unzip3)

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
 {-# INLINE [4] unzip #-}


unzip3  :: ( Shape sh
           , Map r (a, b, c)
           , Unzip (TM r) a (b, c)
           , Unzip (TU (TM r)) b c)
        => Array r sh (a, b, c)
        -> ( Array (TU (TM r)) sh a
           , Array (TU (TU (TM r))) sh b
           , Array (TU (TU (TM r))) sh c)

unzip3 arr
 = let  bake (x, y, z)  = (x, (y, z))
        arr'            = R.map bake arr
        (arr1, arr1')   = unzip arr'
        (arr2, arr3)    = unzip arr1'
   in   (arr1, arr2, arr3)
{-# INLINE unzip3 #-}
