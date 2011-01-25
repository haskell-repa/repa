{-# LANGUAGE BangPatterns #-}

-- | Functions specialised for arrays of dimension 2.
module Data.Array.Repa.Specialised.Dim2
	( isInside2
	, isOutside2
	, clampToBorder2)
where
import Data.Array.Repa


-- | Check if an index lies inside the given extent.
--   As opposed to `inRange` from "Data.Array.Repa.Index", 
--   this is a short-circuited test that checks that lowest dimension first.
isInside2 
	:: DIM2 	-- ^ Extent of array.
	-> DIM2 	-- ^ Index to check.
	-> Bool	

{-# INLINE isInside2 #-}
isInside2 ex 	= not . isOutside2 ex


-- | Check if an index lies outside the given extent.
--   As opposed to `inRange` from "Data.Array.Repa.Index",
--   this is a short-circuited test that checks the lowest dimension first.
isOutside2 
	:: DIM2		-- ^ Extent of array. 
	-> DIM2		-- ^ Index to check.
	-> Bool
	
{-# INLINE isOutside2 #-}
isOutside2 (_ :. yLen :. xLen) (_ :. yy :. xx) 
	| xx < 0	= True
	| xx >= xLen	= True
	| yy < 0	= True
	| yy >= yLen	= True
	| otherwise	= False


-- | Given the extent of an array, clamp the components of an index so they
--   lie within the given array. Outlying indices are clamped to the index
--   of the nearest border element.
clampToBorder2 
	:: DIM2 	-- ^ Extent of array.
	-> DIM2		-- ^ Index to clamp.
	-> DIM2

{-# INLINE clampToBorder2 #-}
clampToBorder2 (_ :. yLen :. xLen) (sh :. j :. i)
 = clampX j i
 where 	{-# INLINE clampX #-}
	clampX !y !x
	  | x < 0	= clampY y 0
	  | x >= xLen	= clampY y (xLen - 1)
	  | otherwise	= clampY y x
		
	{-# INLINE clampY #-}
	clampY !y !x
	  | y < 0	= sh :. 0	   :. x
	  | y >= yLen	= sh :. (yLen - 1) :. x
	  | otherwise	= sh :. y	   :. x


