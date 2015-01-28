{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Internals.Flat
        ( Flat   (..)
        , Bulk1
        , Vector
        , vfromList)
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.RowWise
import Data.Repa.Array.Internals.Target
import Data.Repa.Fusion.Unpack
import Prelude                                  as P


-- | The Flat type overlays a RowWise layout onto some underlying
--   primitive, linear representation.
data Flat r sh 
        = Flat r sh


-- The elements of Flat arrays are stored in row-wise order.
instance (Layout (RowWise sh))
      =>  Layout (Flat r  sh) where

        type Index (Flat r sh)          = Index     (RowWise sh)
        extent     (Flat _ sh)          = extent    (RowWise sh)
        toIndex    (Flat _ sh) ix       = toIndex   (RowWise sh) ix
        fromIndex  (Flat _ sh) n        = fromIndex (RowWise sh) n
        {-# INLINE toIndex #-}
        {-# INLINE extent #-}
        {-# INLINE fromIndex #-}


-- The elements of Flat arrays are stored in some 
-- underlying linear representation.
instance ( Layout (RowWise sh)
         , Bulk r a, Index r ~ Int)
      =>  Bulk (Flat r sh) a where
        
        data Array (Flat r sh) a        = Array sh (Array r a)
        layout (Array sh inner)         = Flat (layout inner) sh
        index  (Array sh inner) ix      = index inner (toIndex (RowWise sh) ix)
        {-# INLINE layout #-}
        {-# INLINE index  #-}


-- When constructing flat arrays we use the same buffer
-- type as the underlying linear representation.
instance ( Layout (RowWise sh), Target r a)
      =>   Target (Flat r sh) a where

        data Buffer (Flat r sh) a
                = FlatBuffer sh (Buffer r a)

        unsafeNewBuffer   (Flat r sh)   
         = do   buf     <- unsafeNewBuffer r
                return  $ FlatBuffer sh buf

        unsafeWriteBuffer  buf ix x     = unsafeWriteBuffer buf ix x
        unsafeGrowBuffer   buf ix       = unsafeGrowBuffer  buf ix
        unsafeSliceBuffer  st  sz buf   = unsafeSliceBuffer st sz buf

        unsafeFreezeBuffer (FlatBuffer sh buf)
         = do   inner   <- unsafeFreezeBuffer buf
                return  $ Array sh inner

        touchBuffer (FlatBuffer _ buf)  = touchBuffer buf
        {-# INLINE unsafeNewBuffer    #-}
        {-# INLINE unsafeWriteBuffer  #-}
        {-# INLINE unsafeGrowBuffer   #-}
        {-# INLINE unsafeSliceBuffer  #-}
        {-# INLINE unsafeFreezeBuffer #-}
        {-# INLINE touchBuffer        #-}


instance Unpack (Buffer r a) tBuf
      => Unpack (Buffer (Flat r sh) a) (sh, tBuf) where

 unpack (FlatBuffer sh buf)             = (sh, unpack buf)
 repack (FlatBuffer _ buf) (sh, ubuf)   = FlatBuffer sh (repack buf ubuf)
 {-# INLINE unpack #-}
 {-# INLINE repack #-}


-------------------------------------------------------------------------------
type Bulk1 r a
        = Bulk  (Flat r DIM1) a

type Vector r a
        = Array (Flat r DIM1) a


-- | O(length src). Construct a vector from a list.
vfromList :: Target (Flat r DIM1) a 
          => r -> [a] -> Vector r a
vfromList r xx
 = let  !len     = P.length xx 
        Just arr = fromList (Flat r (Z :. len)) xx
   in   arr
{-# NOINLINE vfromList #-}

