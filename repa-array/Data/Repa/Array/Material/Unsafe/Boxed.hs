
module Data.Repa.Array.Material.Unsafe.Boxed
        ( UB(..), B(..)
        , Array  (..)
        , Buffer (..)
        , Window (..))
where
import Data.Repa.Array.Window
import Data.Repa.Array.Checked
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Bulk
import qualified Data.Vector            as V

-------------------------------------------------------------------------------
-- | Representation tag for Unsafe arrays of Boxed elements.
data UB = UB


-- | Representation tag for arrays of Boxed elements.
data B  = B


-------------------------------------------------------------------------------
-- | Boxed arrays.
instance Repr B where
 type Safe    B = B
 type Unsafe  B = UB
 repr = B
 {-# INLINE repr #-}


-- | Unsafe boxed arrays.
instance Repr UB where
 type Safe   UB = B
 type Unsafe UB = UB
 repr = UB
 {-# INLINE repr #-}


-------------------------------------------------------------------------------
-- | Unsafe Boxed arrays.
instance Shape sh => Bulk UB sh a where
 data Array UB sh a             = UBArray sh !(V.Vector a)
 extent (UBArray sh _ )         = sh
 index  (UBArray sh vec) ix     = vec `V.unsafeIndex` (toIndex sh ix)
 safe   arr                     = BArray $ checked arr
 unsafe arr                     = arr
 {-# INLINE extent #-}
 {-# INLINE index  #-}
 {-# INLINE safe   #-}
 {-# INLINE unsafe #-}

deriving instance (Show sh, Show a) => Show (Array UB sh a)


-- | Boxed arrays.
instance Shape sh => Bulk B sh a where
 data Array B sh a              = BArray (Array (K UB) sh a)
 index (BArray inner) ix        = index inner ix
 extent (BArray inner)          = extent inner
 safe  arr                      = arr
 unsafe (BArray (KArray inner)) = inner
 {-# INLINE index  #-}
 {-# INLINE extent #-}
 {-# INLINE safe   #-}
 {-# INLINE unsafe #-}

deriving instance (Show sh, Show a) => Show (Array B sh a)

