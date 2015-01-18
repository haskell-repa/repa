
-- | Index space transformation between arrays and slices.
module Data.Repa.Array.Shape.Slice
	( All		(..)
	, Any		(..)
	, FullShape
	, SliceShape
	, Slice		(..))
where
import Data.Repa.Array.Shape.Index
import Prelude		        hiding (replicate, drop)


-- | Select all indices at a certain position.
data All 	= All


-- | Place holder for any possible shape.
data Any sh	= Any


-- | Map a type of the index in the full shape, to the type of the index in the slice.
type family FullShape ss
type instance FullShape Z		= Z
type instance FullShape (Any sh)	= sh
type instance FullShape (sl :. Int)	= FullShape sl :. Int
type instance FullShape (sl :. All)	= FullShape sl :. Int


-- | Map the type of an index in the slice, to the type of the index in the full shape.
type family SliceShape ss
type instance SliceShape Z		= Z
type instance SliceShape (Any sh)	= sh
type instance SliceShape (sl :. Int)	= SliceShape sl
type instance SliceShape (sl :. All)	= SliceShape sl :. Int


-- | Class of index types that can map to slices.
class Slice ss where
	-- | Map an index of a full shape onto an index of some slice.
	sliceOfFull	:: ss -> FullShape ss  -> SliceShape ss

	-- | Map an index of a slice onto an index of the full shape.
	fullOfSlice	:: ss -> SliceShape ss -> FullShape  ss


instance Slice Z  where
	sliceOfFull _ _		= Z
        {-# INLINE [1] sliceOfFull #-}

	fullOfSlice _ _		= Z
        {-# INLINE [1] fullOfSlice #-}


instance Slice (Any sh) where
	sliceOfFull _ sh	= sh
        {-# INLINE [1] sliceOfFull #-}

	fullOfSlice _ sh	= sh
        {-# INLINE [1] fullOfSlice #-}


instance Slice sl => Slice (sl :. Int) where
	sliceOfFull (fsl :. _) (ssl :. _)
		= sliceOfFull fsl ssl
        {-# INLINE [1] sliceOfFull #-}

	fullOfSlice (fsl :. n) ssl
		= fullOfSlice fsl ssl :. n
        {-# INLINE [1] fullOfSlice #-}


instance Slice sl => Slice (sl :. All) where
	sliceOfFull (fsl :. All) (ssl :. s)
		= sliceOfFull fsl ssl :. s
        {-# INLINE [1] sliceOfFull #-}

	fullOfSlice (fsl :. All) (ssl :. s)
		= fullOfSlice fsl ssl :. s
        {-# INLINE [1] fullOfSlice #-}

