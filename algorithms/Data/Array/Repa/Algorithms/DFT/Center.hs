
-- | Applying these transforms to the input of a DFT causes the output 
--   to be centered so that the zero frequency is in the middle. 
module Data.Array.Repa.Algorithms.DFT.Center
	( centerVector
	, centerMatrix)
where
import Data.Array.Repa
import Data.Array.Repa.Algorithms.Complex


-- | Apply the centering transform to a vector.
centerVector
	:: Array DIM1 Complex
	-> Array DIM1 Complex

{-# INLINE centerVector #-}
centerVector arr
 = traverse arr id
	(\get ix@(_ :. x) -> ((-1) ^ x) * get ix)


-- | Apply the centering transform to a matrix.
centerMatrix
	:: Array DIM2 Complex
	-> Array DIM2 Complex

{-# INLINE centerMatrix #-}
centerMatrix arr
 = traverse arr id
	(\get ix@(_ :. y :. x) -> ((-1) ^ (y + x)) * get ix)
