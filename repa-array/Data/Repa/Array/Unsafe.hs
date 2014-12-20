
module Data.Repa.Array.Unsafe
        (Unsafe (..))
where
import Data.Repa.Array.Foreign
import Data.Repa.Array.Unboxed
import Data.Repa.Array.Unsafe.Foreign
import Data.Repa.Array.Unsafe.Unboxed


class Unsafe r1 r2 where
 unsafe :: Array r1 sh a -> Array r2 sh a

instance Unsafe F UF where
 unsafe (FArray sh offset fptr) 
        = (UFArray sh offset fptr)
 {-# INLINE unsafe #-}

instance Unsafe U UU where
 unsafe (UArray sh vec)  
        = (UUArray sh vec)
 {-# INLINE unsafe #-}
