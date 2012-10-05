
module Data.Array.Repa.Vector.Base
        ( Vector
        , Map   (..) )
where
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U


type Vector r e 
        = Array r DIM1 e


-- Map ------------------------------------------------------------------------
class Map r a where
 type TM r
 -- | Vector map that will preserve the chain representation of source vector.
 vmap :: (a -> b) -> Vector r a -> Vector (TM r) b


instance U.Unbox e => Map U e where
 type TM U      = D

 vmap f arr
  = case delay arr of
        ADelayed sh g -> ADelayed sh (f . g)
 {-# INLINE [4] vmap #-}


instance Map D e where
 type TM D      = D
 vmap           = R.map
 {-# INLINE [4] vmap #-}

