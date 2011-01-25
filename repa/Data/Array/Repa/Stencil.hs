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
	, mapStencil2

	-- * Template haskell code.
	, stencil2)
where
import Data.Array.Repa			as R
import Data.Array.Repa.Stencil.Base
import Data.Array.Repa.Stencil.Template
import Data.Array.Repa.Specialised.Dim2
import qualified Data.Array.Repa.Shape	as S
import qualified Data.Vector.Unboxed	as V
import Data.List			as List
import GHC.Exts
import Debug.Trace


-- | Apply a stencil to every element of an array.
--   This is specialised for stencils of extent up to 5x5.
mapStencil2 
	:: (Elt a, Elt b)
	=> Boundary a
	-> Stencil DIM2 a b
	-> Array DIM2 a -> Array DIM2 b

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
	
	-- range of values where we don't need to worry about the border
	rngInternal	= ( Z :. yMin :. xMin
			  , Z :. yMax :. xMax )

	-- range of values where some of the data needed by the stencil is outside the image.
	rngsBorder
	 = 	[ ((Z :. 0        :. 0),        (Z :. yMin -1        :. aWidth - 1))	-- bot 
	   	, ((Z :. yMax + 1 :. 0),        (Z :. aHeight - 1    :. aWidth - 1)) 	-- top
		, ((Z :. yMin     :. 0),        (Z :. yMax           :. xMin - 1))	-- left
	   	, ((Z :. yMin     :. xMax + 1), (Z :. yMax           :. aWidth - 1)) ]  -- right
			
	{-# INLINE getBorder' #-}
	getBorder' ix	= unsafeAppStencilBorder2   boundary stencil arr ix
			
	{-# INLINE getInner' #-}	
	getInner' ix	= unsafeAppStencilInternal2 stencil arr ix
						
   in	Partitioned (extent arr) (const False)
		rngsBorder  getBorder'
		rngInternal getInner'


-- | Apply a stencil to a single, internal position in an image.
--	Applying it too close to the border yields badness.
--	TODO: force delayed arrays before processing them.
unsafeAppStencilInternal2
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b -> Array DIM2 a -> DIM2 -> b

{-# INLINE unsafeAppStencilInternal2 #-}
unsafeAppStencilInternal2 
	stencil@(StencilStatic sExtent zero load)
	    arr@(Manifest aExtent vec) 
	     ix@(Z :. y :. x)

	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 5, sWidth <= 5
	= let	-- We want to go access the vector directly here so we don't 
		-- have to calculate incides for every access of the source array.
		!center	= x + y * aWidth

		-- Build a function to pass data from the array to our stencil.
		{-# INLINE oload #-}
		oload oy ox	
		 = let	!ix'	= Z :. oy :. ox
		   in	load ix' (vec `V.unsafeIndex` (center + ox + oy * aWidth))
	
	   in	template5x5 oload zero
	
	| otherwise
	= error "unsafeAppStencil2: finish this for larger stencils"
		

-- | Apply a stencil to a single position in an image.
--	This version can be applied close to the border, or even outside the image.
unsafeAppStencilBorder2
	:: (Elt a, Elt b)
	=> Boundary a
	-> Stencil DIM2 a b
	-> Array DIM2 a
	-> DIM2 -> b

{-# INLINE unsafeAppStencilBorder2 #-}
unsafeAppStencilBorder2	boundary
	 stencil@(StencilStatic  sExtent zero load)
	     arr@(Manifest aExtent vec)
	ix@(Z :. y :. x)
	
	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 5, sWidth <= 5
	= let
		{-# INLINE oload #-}
		-- takes the offset into the stencil, produces data from the input array.
		oload oy ox
		 = let	!yy		= y + oy
			!xx 		= x + ox
			!ixStencil	= Z :. oy :. ox
			!ixArray	= Z :. yy :. xx
			
			result
			 | False	<- isOutside2 ixArray (extent arr)
			 = load ixStencil (arr `unsafeIndex` ixArray)
			
			 | BoundConst bZero	<- boundary
			 = load ixStencil bZero
			
			 | BoundClamp		<- boundary
			 , ixArray_clamped	<- clampToBorder2 (extent arr) ixArray
			 = load ixStencil (arr `unsafeIndex` ixArray_clamped)
			
		   in	result		

	  in	template5x5 oload zero
	
	| otherwise
	= error "unsafeAppStencil2: finish this for larger stencils"


	
-- | Data template for stencils up to 5x5.
--   TODO: we probably don't need to statically define this if we're using TH to defined the stencils.
template5x5
	:: (Int -> Int -> a -> a)
	-> a -> a

{-# INLINE template5x5 #-}
template5x5 f zero
 	= f (-2) (-2)  $  f (-2) (-1)  $  f (-2)   0  $  f (-2)   1  $  f (-2)   2 
	$ f (-1) (-2)  $  f (-1) (-1)  $  f (-1)   0  $  f (-1)   1  $  f (-1)   2 
	$ f   0  (-2)  $  f   0  (-1)  $  f   0    0  $  f   0    1  $  f   0    2  
	$ f   1  (-2)  $  f   1  (-1)  $  f   1    0  $  f   1    1  $  f   1    2 
	$ f   2  (-2)  $  f   2  (-1)  $  f   2    0  $  f   2    1  $  f   2    2 
	$ zero



		

