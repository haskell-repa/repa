{-# LANGUAGE BangPatterns #-}

-- | Functions specialised for arrays of dimension 2.
module Data.Array.Repa.Specialised.Dim2
	( isInside2
	, isOutside2
	, clampToBorder2
	, makeBordered2)
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


-- | Make a 2D partitioned array given two generators, one to produce elements in the
--   border region, and one to produce values in the internal region.
--   The border must be the same width on all sides.
makeBordered2
	:: Elt a
	=> DIM2			-- ^ Extent of array.
	-> Int			-- ^ Width of border.
	-> Generator DIM2 a	-- ^ Generator for border elements.
	-> Generator DIM2 a	-- ^ Generator for internal elements.
	-> Array DIM2 a

{-# INLINE makeBordered2 #-}
makeBordered2 sh@(_ :. aHeight :. aWidth) borderWidth genInternal genBorder
 = let
	-- minimum and maximum indicies of values in the inner part of the image.
	!xMin		= borderWidth
	!yMin		= borderWidth
	!xMax		= aWidth  - borderWidth  - 1
	!yMax		= aHeight - borderWidth - 1

	-- | Range of values where some of the data needed by the stencil is outside the image.
	rectsBorder
	 = 	[ Rect (Z :. 0        :. 0)        (Z :. yMin -1        :. aWidth - 1)		-- bot
	   	, Rect (Z :. yMax + 1 :. 0)        (Z :. aHeight - 1    :. aWidth - 1)	 	-- top
		, Rect (Z :. yMin     :. 0)        (Z :. yMax           :. xMin - 1)		-- left
	   	, Rect (Z :. yMin     :. xMax + 1) (Z :. yMax           :. aWidth - 1) ]  	-- right

	{-# INLINE inBorder #-}
	inBorder 	= not . inInternal

	-- Range of values where we don't need to worry about the border
	rectsInternal
	 = 	[ Rect (Z :. yMin :. xMin)	   (Z :. yMax :. xMax ) ]

	{-# INLINE inInternal #-}
	inInternal (Z :. y :. x)
		=  x >= xMin && x <= xMax
		&& y >= yMin && y <= yMax

   in	Array sh
		[ Region (RangeRects inBorder   rectsBorder)    genInternal
		, Region (RangeRects inInternal rectsInternal)  genBorder ]

