module Data.Vector.Repa.Base
        ( Vector
        , Map(..)
        , Zip(..)
        , vlength)
where
import Data.Array.Repa                  as R
import Prelude                          hiding (length)
import qualified Data.Vector.Unboxed    as U
import Prelude hiding (zip, map, length, replicate)

type Vector r e 
        = Array r DIM1 e


-- Map ------------------------------------------------------------------------
class Map r a where
 type TM r
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


class Zip r1 r2 a b where
 type TZ r1 r2
 vzip   :: Vector r1 a -> Vector r2 b
        -> Vector (TZ r1 r2) (a, b)



-------------------------------------------------------------------------------
-- | Get the length of a vector.
vlength :: Source r e => Vector r e -> Int
vlength !v
 = case extent v of
        Z :. len        -> len
{-# INLINE [4] vlength #-}
