
-- | Applying these transforms to the input of a DFT causes the output 
--   to be centered so that the zero frequency is in the middle. 
module Data.Array.Repa.Algorithms.DFT.Center
	( center1d
	, center2d
	, center3d)
where
import Data.Array.Repa
import Data.Array.Repa.Algorithms.Complex

-- | Apply the centering transform to a vector.
center1d :: Array DIM1 Complex -> Array DIM1 Complex
{-# INLINE center1d #-}
center1d arr
 = traverse arr id
	(\get ix@(_ :. x) -> ((-1) ^ x) * get ix)


-- | Apply the centering transform to a matrix.
center2d :: Array DIM2 Complex -> Array DIM2 Complex
{-# INLINE center2d #-}
center2d arr
 = traverse arr id
	(\get ix@(_ :. y :. x) -> ((-1) ^ (y + x)) * get ix)


-- | Apply the centering transform to a 3d array.
center3d :: Array DIM3 Complex -> Array DIM3 Complex
{-# INLINE center3d #-}
center3d arr
 = traverse arr id
	(\get ix@(_ :. z :. y :. x) -> ((-1) ^ (z + y + x)) * get ix)
