{-# LANGUAGE 	MagicHash, PatternGuards, BangPatterns, TemplateHaskell, QuasiQuotes,
		ParallelListComp, TypeOperators, ExplicitForAll, ScopedTypeVariables #-}
{-# OPTIONS -Wnot #-}

-- | Efficient computation of stencil based convolutions.
--
--   This is specialised for stencils up to 7x7.
--   Due to limitations in the GHC optimiser, using larger stencils doesn't work, and will yield `error`
--   at runtime. We can probably increase the limit if required -- just ask.
--
--   The focus of the stencil is in the center of the 7x7 tile, which has coordinates (0, 0).
--   All coefficients in the stencil must fit in the tile, so they can be given X,Y coordinates up to
--   +/- 3 positions. The stencil can be any shape, and need not be symmetric -- provided it fits in the 7x7 tile.
--
module Data.Array.Repa.Stencil
	( Stencil	(..)
	, Boundary	(..)

	-- * Stencil creation.
	, makeStencil, makeStencil2

	-- * Stencil operators.
	, mapStencil2,     forStencil2
	, mapStencilFrom2, forStencilFrom2

	--  From Data.Array.Repa.Stencil.Template
	, stencil2)
where
import Data.Array.Repa			as R
import Data.Array.Repa.Internals.Base	as R
import Data.Array.Repa.Stencil.Base
import Data.Array.Repa.Stencil.Template
import Data.Array.Repa.Specialised.Dim2
import qualified Data.Array.Repa.Shape	as S
import qualified Data.Vector.Unboxed	as V
import Data.List			as List
import GHC.Exts
import GHC.Base
import Debug.Trace

-- | A index into the flat array.
--   Should be abstract outside the stencil modules.
data Cursor
	= Cursor Int


-- Wrappers ---------------------------------------------------------------------------------------
-- | Like `mapStencil2` but with the parameters flipped.
forStencil2
	:: Elt a
	=> Boundary a
	-> Array DIM2 a
	-> Stencil DIM2 a
	-> Array DIM2 a

{-# INLINE forStencil2 #-}
forStencil2 boundary arr stencil
	= mapStencil2 boundary stencil arr


-- | Like `mapStencilFrom2` but with the parameters flipped.
forStencilFrom2
	:: (Elt a, Elt b)
	=> Boundary a
	-> Array DIM2 b
	-> (b -> a)
	-> Stencil DIM2 a
	-> Array DIM2 a

{-# INLINE forStencilFrom2 #-}
forStencilFrom2 boundary arr from stencil
	= mapStencilFrom2 boundary stencil arr from


-- | Apply a stencil to every element of a 2D array.
--   The array must be manifest else `error`.
mapStencil2
	:: Elt a
	=> Boundary a
	-> Stencil DIM2 a
	-> Array DIM2 a
	-> Array DIM2 a

{-# INLINE mapStencil2 #-}
mapStencil2 boundary stencil arr
	= mapStencilFrom2 boundary stencil arr id


---------------------------------------------------------------------------------------------------
-- | Apply a stencil to every element of a 2D array.
--   The array must be manifest else `error`.
mapStencilFrom2
	:: (Elt a, Elt b)
	=> Boundary a		-- ^ How to handle the boundary of the array.
	-> Stencil DIM2 a	-- ^ Stencil to apply.
	-> Array DIM2 b		-- ^ Array to apply stencil to.
	-> (b -> a)		-- ^ Apply this function to values read from the array before
				--   transforming them with the stencil.
	-> Array DIM2 a

{-# INLINE mapStencilFrom2 #-}
mapStencilFrom2 boundary stencil@(StencilStatic sExtent zero load) arr preConvert
 = let	(_ :. aHeight :. aWidth) = extent arr
	(_ :. sHeight :. sWidth) = sExtent

	sHeight2	= sHeight `div` 2
	sWidth2		= sWidth  `div` 2

	-- minimum and maximum indicies of values in the inner part of the image.
	!xMin		= sWidth2
	!yMin		= sHeight2
	!xMax		= aWidth  - sWidth2  - 1
	!yMax		= aHeight - sHeight2 - 1

	-- Rectangles -----------------------
	-- range of values where we don't need to worry about the border
	rectsInternal
	 = 	[ Rect (Z :. yMin :. xMin)	   (Z :. yMax :. xMax ) ]

	{-# INLINE inInternal #-}
	inInternal (Z :. y :. x)
		=  x >= xMin && x <= xMax
		&& y >= yMin && y <= yMax


	-- range of values where some of the data needed by the stencil is outside the image.
	rectsBorder
	 = 	[ Rect (Z :. 0        :. 0)        (Z :. yMin -1        :. aWidth - 1)		-- bot
	   	, Rect (Z :. yMax + 1 :. 0)        (Z :. aHeight - 1    :. aWidth - 1)	 	-- top
		, Rect (Z :. yMin     :. 0)        (Z :. yMax           :. xMin - 1)		-- left
	   	, Rect (Z :. yMin     :. xMax + 1) (Z :. yMax           :. aWidth - 1) ]  	-- right

	{-# INLINE inBorder #-}
	inBorder 	= not . inInternal


	-- Cursor functions ----------------
	{-# INLINE makeCursor' #-}
	makeCursor' (Z :. y :. x)
	 = Cursor (x + y * aWidth)

	{-# INLINE shiftCursor' #-}
	shiftCursor' ix (Cursor off)
	 = Cursor
	 $ case ix of
		Z :. y :. x	-> off + y * aWidth + x

	{-# INLINE getInner' #-}
	getInner' cur
	 = unsafeAppStencilCursor2 shiftCursor' stencil
		arr preConvert cur

	{-# INLINE getBorder' #-}
	getBorder' cur
	 = case boundary of
		BoundConst c	-> c
		BoundClamp 	-> unsafeAppStencilCursor2_clamp addDim stencil
					arr preConvert cur

   in	Array (extent arr)
		[ Region (RangeRects inBorder rectsBorder)
			 (GenCursor id addDim getBorder')

		, Region (RangeRects inInternal rectsInternal)
		     	 (GenCursor makeCursor' shiftCursor' getInner') ]


unsafeAppStencilCursor2
	:: (Elt a, Elt b)
	=> (DIM2 -> Cursor -> Cursor)
	-> Stencil DIM2 a
	-> Array DIM2 b
	-> (b -> a)
	-> Cursor
	-> a

{-# INLINE [1] unsafeAppStencilCursor2 #-}
unsafeAppStencilCursor2 shift
	stencil@(StencilStatic sExtent zero load)
	    arr@(Array aExtent [Region RangeAll (GenManifest vec)]) preConvert
	    cur@(Cursor off)

	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 7, sWidth <= 7
	= let
		-- Get data from the manifest array.
		{-# INLINE [0] getData #-}
		getData (Cursor cur) = preConvert $ vec `V.unsafeIndex` cur

		-- Build a function to pass data from the array to our stencil.
		{-# INLINE oload #-}
		oload oy ox
		 = let	!cur' = shift (Z :. oy :. ox) cur
		   in	load (Z :. oy :. ox) (getData cur')

	   in	template7x7 oload zero


-- | Like above, but clamp out of bounds array values to the closest real value.
unsafeAppStencilCursor2_clamp
	:: forall a b. (Elt a, Elt b)
	=> (DIM2 -> DIM2 -> DIM2)
	-> Stencil DIM2 a
	-> Array DIM2 b
	-> (b -> a)
	-> DIM2
	-> a

{-# INLINE [1] unsafeAppStencilCursor2_clamp #-}
unsafeAppStencilCursor2_clamp shift
	   stencil@(StencilStatic sExtent zero load)
	       arr@(Array aExtent [Region RangeAll (GenManifest vec)]) preConvert
	       cur

	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 7, sWidth <= 7
	= let
		-- Get data from the manifest array.
		{-# INLINE [0] getData #-}
		getData :: DIM2 -> a
		getData (Z :. y :. x)
		 = wrapLoadX x y

		-- TODO: Inlining this into above makes SpecConstr choke
		wrapLoadX :: Int -> Int -> a
		wrapLoadX !x !y
		 | x < 0	= wrapLoadY 0      	 y
		 | x >= aWidth	= wrapLoadY (aWidth - 1) y
		 | otherwise    = wrapLoadY x y

		{-# INLINE wrapLoadY #-}
		wrapLoadY :: Int -> Int -> a
		wrapLoadY !x !y
		 | y <  0	= loadXY x 0
		 | y >= aHeight = loadXY x (aHeight - 1)
		 | otherwise    = loadXY x y

		{-# INLINE loadXY #-}
		loadXY :: Int -> Int -> a
		loadXY !x !y
		 = preConvert $ vec `V.unsafeIndex` (x + y * aWidth)

		-- Build a function to pass data from the array to our stencil.
		{-# INLINE oload #-}
		oload oy ox
		 = let	!cur' = shift (Z :. oy :. ox) cur
		   in	load (Z :. oy :. ox) (getData cur')

	   in	template7x7 oload zero



-- | Data template for stencils up to 7x7.
template7x7
	:: (Int -> Int -> a -> a)
	-> a -> a

{-# INLINE [1] template7x7 #-}
template7x7 f zero
 	= f (-3) (-3)  $  f (-3) (-2)  $  f (-3) (-1)  $  f (-3)   0  $  f (-3)   1  $  f (-3)   2  $ f (-3) 3
 	$ f (-2) (-3)  $  f (-2) (-2)  $  f (-2) (-1)  $  f (-2)   0  $  f (-2)   1  $  f (-2)   2  $ f (-2) 3
	$ f (-1) (-3)  $  f (-1) (-2)  $  f (-1) (-1)  $  f (-1)   0  $  f (-1)   1  $  f (-1)   2  $ f (-1) 3
	$ f   0  (-3)  $  f   0  (-2)  $  f   0  (-1)  $  f   0    0  $  f   0    1  $  f   0    2  $ f   0  3
	$ f   1  (-3)  $  f   1  (-2)  $  f   1  (-1)  $  f   1    0  $  f   1    1  $  f   1    2  $ f   1  3
	$ f   2  (-3)  $  f   2  (-2)  $  f   2  (-1)  $  f   2    0  $  f   2    1  $  f   2    2  $ f   2  3
	$ f   3  (-3)  $  f   3  (-2)  $  f   3  (-1)  $  f   3    0  $  f   3    1  $  f   3    2  $ f   3  3
	$ zero

