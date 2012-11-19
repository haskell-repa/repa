
module Data.Array.Repa.Vector.Operators.Map
        (Map(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U
import Prelude                          hiding (map)


class Map r a where
 type TM r
 -- | Vector map where the representation of the result depends on the 
 --   representation of the source.
 map :: (a -> b) -> Vector r a -> Vector (TM r) b


instance U.Unbox a => Map U a where
 type TM U      = D

 map f arr
  = case delay arr of
        ADelayed sh g -> ADelayed sh (f . g)
 {-# INLINE [4] map #-}


instance Map D e where
 type TM D      = D
 map            = R.map
 {-# INLINE [4] map #-}


