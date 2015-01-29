{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Internals.Dense
        ( Dense (..)
        , Bulk1
        , Vector
        , vfromList)
where
import Data.Repa.Array.Index
import Data.Repa.Array.RowWise
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Prelude                                  as P


-- | The Dense layout maps a higher-ranked index space to some underlying
--   linear index space.
data Dense r l 
        = Dense r l


-- The elements of Flat arrays are stored in row-wise order.
instance (Index r ~ Int, Layout l)
      =>  Layout (Dense r l) where

        type Index (Dense r l)          = Index     l
        extent     (Dense _ l)          = extent    l
        toIndex    (Dense _ l) ix       = toIndex   l ix
        fromIndex  (Dense _ l) n        = fromIndex l n
        {-# INLINE extent #-}
        {-# INLINE toIndex #-}
        {-# INLINE fromIndex #-}


-- The elements of Flat arrays are stored in some 
-- underlying linear representation.
instance (Index r ~ Int, Layout l, Bulk r a)
      =>  Bulk (Dense r l) a where
        
        data Array (Dense r l) a        = Array l (Array r a)
        layout (Array l inner)          = Dense (layout inner) l
        index  (Array l inner) ix       = index inner (toIndex l ix)
        {-# INLINE layout #-}
        {-# INLINE index  #-}


-- When constructing flat arrays we use the same buffer
-- type as the underlying linear representation.
instance (Index r ~ Int, Layout l, Target r a)
      =>  Target (Dense r l) a where

        data Buffer (Dense r l) a
                = DenseBuffer l (Buffer r a)

        unsafeNewBuffer   (Dense r l)   
         = do   buf     <- unsafeNewBuffer r
                return  $ DenseBuffer l buf

        unsafeWriteBuffer  buf ix x     = unsafeWriteBuffer buf ix x
        unsafeGrowBuffer   buf ix       = unsafeGrowBuffer  buf ix
        unsafeSliceBuffer  st  sz buf   = unsafeSliceBuffer st sz buf

        unsafeFreezeBuffer (DenseBuffer l buf)
         = do   inner   <- unsafeFreezeBuffer buf
                return  $ Array l inner

        touchBuffer (DenseBuffer _ buf)  = touchBuffer buf
        {-# INLINE unsafeNewBuffer    #-}
        {-# INLINE unsafeWriteBuffer  #-}
        {-# INLINE unsafeGrowBuffer   #-}
        {-# INLINE unsafeSliceBuffer  #-}
        {-# INLINE unsafeFreezeBuffer #-}
        {-# INLINE touchBuffer        #-}


instance Unpack (Buffer r a) tBuf
      => Unpack (Buffer (Dense r l) a) (l, tBuf) where

 unpack (DenseBuffer l buf)             = (l, unpack buf)
 repack (DenseBuffer _ buf) (l, ubuf)   = DenseBuffer l (repack buf ubuf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-------------------------------------------------------------------------------
type Bulk1 r a
        = (Bulk (Dense r DIM1) a)

type Vector r a
        = Array (Dense r DIM1) a


-- | O(length src). Construct a vector from a list.
vfromList :: Target (Dense r DIM1) a 
          => r -> [a] -> Vector r a
vfromList r xx
 = let  !len     = P.length xx 
        Just arr = fromList (Dense r (RowWise (Z :. len))) xx
   in   arr
{-# NOINLINE vfromList #-}

