
module Data.Array.Repa.Vector.Operators.Map
        (Map(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Repr.Flow
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import Prelude                                  hiding (map)


class Map r a where
 type TM r
 -- | Vector map where the representation of the result depends on the 
 --   representation of the source.
 map    :: (a -> b)    -> Vector r a   -> Vector (TM r) b


-- Unboxed
instance (Elt a, U.Unbox a) => Map U a where
 type TM U      = D

 map f arr
  = case delay arr of
        ADelayed sh g -> ADelayed sh (f . g)
 {-# INLINE [4] map #-}


-- Delayed
instance Map D a where
 type TM D      = D
 map f arr
   = case delay arr of
        ADelayed sh g    -> ADelayed sh (f . g)
 {-# INLINE [4] map #-}


-- Flows
instance Map (O mode dist) a where
 type TM (O mode dist)
       = (O mode dist)

 map f (AFlow ff)
        = AFlow (F.map f ff)
 {-# INLINE [4] map #-}


