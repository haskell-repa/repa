{-# LANGUAGE 	MagicHash, PatternGuards, BangPatterns, TemplateHaskell, QuasiQuotes, 
		ParallelListComp, TypeOperators #-}
{-# OPTIONS -Wnot #-}

-- | Efficient computation of stencil based convolutions.
--   TODO: Also handle stencils larger than 5x5.
module Data.Array.Repa.Stencil
	( Stencil	(..)
	, Boundary	(..)

	-- * Stencil creation.
	, makeStencil, makeStencil2

	-- * Stencil operators.
	, mapStencil2, forStencil2

	-- * Template haskell code.
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
import Debug.Trace

-- | A index into the flat array.
--   Should be abstract outside the stencil modules.
data Cursor 
	= Cursor Int


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


-- | Apply a stencil to every element of an array.
--   This is specialised for stencils of extent up to 5x5.
mapStencil2 
	:: Elt a
	=> Boundary a
	-> Stencil DIM2 a
	-> Array DIM2 a -> Array DIM2 a

{-# INLINE mapStencil2 #-}
mapStencil2 boundary stencil@(StencilStatic sExtent zero load) arr
 = let	(_ :. aHeight :. aWidth) = extent arr
	(_ :. sHeight :. sWidth) = sExtent

	sHeight2	= sHeight `div` 2
	sWidth2		= sWidth  `div` 2

	-- minimum and maximum indicies of values in the inner part of the image.
	xMin		= sWidth2
	yMin		= sHeight2
	xMax		= aWidth  - sWidth2  - 1
	yMax		= aHeight - sHeight2 - 1

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
	makeCursor'   (Z :. y :. x)	
			= Cursor (x + y * aWidth)
	
	{-# INLINE shiftCursor' #-}
	shiftCursor' ix (Cursor off)
	 = Cursor
	 $ case ix of
		Z :. y :. x	-> off + y * aWidth + x
			
	{-# INLINE getInner' #-}
	getInner' cur	= unsafeAppStencilCursor2 stencil arr shiftCursor' cur
	
	{-# INLINE getBorder' #-}
	getBorder' _	= zero
							
   in	Array (extent arr)
		[ Region (RangeRects inBorder rectsBorder)
			 (GenCursor id addDim getBorder')
			
		, Region (RangeRects inInternal rectsInternal)
		    	(GenCursor makeCursor' shiftCursor' getInner') ]



unsafeAppStencilCursor2
	:: Elt a
	=> Stencil DIM2 a
	-> Array DIM2 a 
	-> (DIM2 -> Cursor -> Cursor)	-- offsetCursor
	-> Cursor 
	-> a

{-# INLINE [1] unsafeAppStencilCursor2 #-}
unsafeAppStencilCursor2
	    stencil@(StencilStatic sExtent zero load)
	        arr@(Array aExtent [Region RangeAll (GenManifest vec)])
	 offsetCursor
	     cur@(Cursor off)

	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 5, sWidth <= 5
	= let	
		-- Get data from the manifest array.
		{-# INLINE [0] getData #-}
		getData (Cursor cur)
			= vec `V.unsafeIndex` cur
		
		-- Build a function to pass data from the array to our stencil.
		{-# INLINE oload #-}
		oload oy ox	
		 = let	!cur' = offsetCursor (Z :. oy :. ox) cur
		   in	load (Z :. oy :. ox) (getData cur')
	
	   in	template5x5 oload zero


-- | Data template for stencils up to 5x5.
template5x5
	:: (Int -> Int -> a -> a)
	-> a -> a

{-# INLINE [1] template5x5 #-}
template5x5 f zero
 	= f (-2) (-2)  $  f (-2) (-1)  $  f (-2)   0  $  f (-2)   1  $  f (-2)   2 
	$ f (-1) (-2)  $  f (-1) (-1)  $  f (-1)   0  $  f (-1)   1  $  f (-1)   2 
	$ f   0  (-2)  $  f   0  (-1)  $  f   0    0  $  f   0    1  $  f   0    2  
	$ f   1  (-2)  $  f   1  (-1)  $  f   1    0  $  f   1    1  $  f   1    2 
	$ f   2  (-2)  $  f   2  (-1)  $  f   2    0  $  f   2    1  $  f   2    2 
	$ zero

