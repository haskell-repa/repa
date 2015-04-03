
-- | Applying these transforms to the input of a DFT causes the output 
--   to be centered so that the zero frequency is in the middle. 
module Data.Array.Repa.Algorithms.DFT.Center
        ( center1d
        , center2d
        , center3d)
where
import Data.Array.Repa                          as R
import Data.Array.Repa.Algorithms.Complex       as R

-- | Apply the centering transform to a vector.
center1d
        :: Source r Complex
        => Array  r DIM1 Complex -> Array D DIM1 Complex
{-# INLINE center1d #-}
center1d arr
 = R.traverse arr id
        (\get ix@(_ :. x) -> ((-1) ^ x) * get ix)


-- | Apply the centering transform to a matrix.
center2d
        :: Source r Complex
        => Array  r DIM2 Complex -> Array D DIM2 Complex
{-# INLINE center2d #-}
center2d arr
 = R.traverse arr id
        (\get ix@(_ :. y :. x) -> ((-1) ^ (y + x)) * get ix)


-- | Apply the centering transform to a 3d array.
center3d 
        :: Source r Complex
        => Array  r DIM3 Complex -> Array D DIM3 Complex
{-# INLINE center3d #-}
center3d arr
 = R.traverse arr id
        (\get ix@(_ :. z :. y :. x) -> ((-1) ^ (z + y + x)) * get ix)
