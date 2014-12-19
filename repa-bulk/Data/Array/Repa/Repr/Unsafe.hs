
module Data.Array.Repa.Repr.Unsafe
        (Unsafe (..))
where
import Data.Array.Repa.Repr.Foreign
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Repr.Unsafe.Foreign
import Data.Array.Repa.Repr.Unsafe.Unboxed

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
