
module Data.Array.Repa.Vector.Map
        (Map (..))
where
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Chain.Map        as C
import Data.Array.Repa.Stream.Map       as S
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U


-- | Vector map perserves the representation of delayed source vectors.
class Map r a where
 type MapR r
 vmap :: (a -> b) -> Vector r a -> Vector (MapR r) b

-- Unboxed
instance U.Unbox e => Map U e where
 type MapR U      = D

 vmap f arr
  = case delay arr of
        ADelayed sh g -> ADelayed sh (f . g)
 {-# INLINE [4] vmap #-}


-- Delayed
instance Map D e where
 type MapR D    = D
 vmap           = R.map
 {-# INLINE [4] vmap #-}


-- Chained
instance Map N a where
 type MapR N   = N
 vmap f (AChain sh dchain arr)
  = AChain sh (C.mapD f dchain) (R.map f arr)
 {-# INLINE vmap #-}


-- Streamed
instance Map S a where
 type MapR S    = S
 vmap f (AStream sh dchain arr)
  = AStream sh (S.mapD f dchain) (R.map f arr)
 {-# INLINE vmap #-}
