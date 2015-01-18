{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Checked
        ( K      (..)
        , Repr   (..)
        , Bulk   (..),   Array  (..)
        , Buffer (..)
        , checked,      unchecked)
where
import Data.Repa.Array.Shape
import Data.Repa.Eval.Array
import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Internals.Bulk
import Control.Monad

-- | Checked arrays are wrappers that perform bounds checks before indexing
--   into a lower level representation.
--
data K r = K r


-- | Checked arrays.
instance Repr r => Repr (K r) where
 repr = K repr

 type Safe   (K r) = K r
 type Unsafe (K r) = r


---------------------------------------------------------------------------------------------------
-- | Checked arrays.
instance Bulk r sh a => Bulk (K r) sh a where
 data Array (K r) sh a
        = KArray (Array r sh a)

 index  (KArray inner) ix
        | not $ inShapeRange zeroDim (extent inner) ix
        = error "repa-array.index out of range"

        | otherwise
        = index inner ix

 extent (KArray inner)  = extent inner
 {-# INLINE extent #-}

 safe arr               = arr
 {-# INLINE safe #-}

 unsafe (KArray inner)  = inner
 {-# INLINE unsafe #-}

deriving instance 
          (Show sh, Show (Array r sh a)) 
        => Show (Array (K r) sh a)


---------------------------------------------------------------------------------------------------
instance (Target r e t, Unpack (Buffer (K r) e) t) 
       => Target (K r) e t where
 data Buffer (K r) e
        = KBuffer (Buffer r e)

 unsafeNewBuffer   len          
        = liftM KBuffer $ unsafeNewBuffer len
 {-# INLINE unsafeNewBuffer #-}

 unsafeWriteBuffer (KBuffer buf) ix e     
        = unsafeWriteBuffer buf ix e
 {-# INLINE unsafeWriteBuffer #-}

 unsafeGrowBuffer  (KBuffer buf) ix
        = liftM KBuffer $ unsafeGrowBuffer buf ix
 {-# INLINE unsafeGrowBuffer #-}

 unsafeSliceBuffer start len (KBuffer buf)
        = liftM KBuffer $ unsafeSliceBuffer start len buf
 {-# INLINE unsafeSliceBuffer #-}

 unsafeFreezeBuffer sh (KBuffer buf)
        = liftM checked $ unsafeFreezeBuffer sh buf
 {-# INLINE unsafeFreezeBuffer #-}

 touchBuffer (KBuffer buf)
        = touchBuffer buf
 {-# INLINE touchBuffer #-}


---------------------------------------------------------------------------------------------------
instance Unpack (Buffer r e) t
      => Unpack (Buffer (K r) e) t where
 unpack (KBuffer buf)   = unpack buf
 {-# INLINE unpack #-}

 repack (KBuffer buf) parts  
        = KBuffer (repack buf parts)
 {-# INLINE repack #-}


-- | O(1). Yield a checked version of an array.
checked   :: Array r sh a     -> Array (K r) sh a
checked arr = KArray arr
{-# INLINE checked #-}


-- | O(1). Yield the unchecked version of an array.
unchecked :: Array (K r) sh a -> Array r sh a
unchecked (KArray arr) = arr
{-# INLINE unchecked #-}

