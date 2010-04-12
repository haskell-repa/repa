{-# OPTIONS -fglasgow-exts #-}

module FlatDim
	( FShape(..)
	, FDIM2(..))
where
import GHC.Exts


-- | A cut down version of the Shape class.
--	These are all the things we've implemented as flat indices.
--	This class also doesn't have the same superclasses as the one in the Array module.
--
class FShape sh where

	-- | Yield the total number of elements in the array.
	size :: sh -> Int

	-- | Corresponding index into a linear representation of the array.
	toIndex :: sh -> sh -> Int

	-- | Given index into linear, row major representation, calculates
	--   the index into the array.
	fromIndex :: sh -> Int -> sh
	

-- DIM2 -------------------------------------------------------------------------------------------
data FDIM2 
	= FDIM2 Int# Int#
	deriving Show


instance FShape FDIM2 where

	{-# INLINE size #-}
	size  (FDIM2 n m)
		= I# (n *# m)

	{-# INLINE toIndex #-}
	toIndex (FDIM2 n m) (FDIM2 i j)	
		= I# (i *# m +# j)

	{-# INLINE fromIndex #-}
	fromIndex (FDIM2 n m) (I# ix)
		= FDIM2 (ix `quotInt#` m) (ix `remInt#` n)
