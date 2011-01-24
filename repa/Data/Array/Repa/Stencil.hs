{-# LANGUAGE MagicHash, PatternGuards, BangPatterns  #-}
{-# OPTIONS -Wnot #-}

-- | Efficient computation of stencil based convolutions.
--   TODO: Also handle stencils larger than 5x5.
module Data.Array.Repa.Stencil
	( Stencil	(..)
	, Boundary	(..)
	, makeStencil
	, mapStencil2)
where
import Data.Array.Repa			as R
import qualified Data.Array.Repa.Shape	as S
import qualified Data.Vector.Unboxed	as V
import GHC.Exts
import Debug.Trace

-- | Represents a stencil we can apply to an array.
data Stencil sh a b
	= StencilStatic
	{ stencilExtent	:: !sh
	, stencilZero	:: !b 
	, stencilAcc	:: !(sh -> a -> b -> b) }


-- | What values to use when the stencil is partly outside the input image.
data Boundary a
	= BoundConst a

-- | Make a stencil from a function yielding coefficients at each index.
makeStencil
	:: (Elt a, Num a) 
	=> sh			-- ^ Extent of stencil.
	-> (sh -> Maybe a) 	-- ^ Get the coefficient at this index.
	-> Stencil sh a a

{-# INLINE makeStencil #-}
makeStencil ex getCoeff
 = StencilStatic ex 0 
 $ \ix val acc
	-> case getCoeff ix of
		Nothing		-> acc
		Just coeff	-> acc + val * coeff
	

-- | Apply a stencil to every element of an array.
--   This is specialised for stencils of extent up to 5x5.
mapStencil2 
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b
	-> Boundary a
	-> Array DIM2 a -> Array DIM2 b

{-# INLINE mapStencil2 #-}
mapStencil2 stencil@(StencilStatic sExtent zero load) boundary arr
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
	getBorder' ix	= unsafeAppStencilBorder2   stencil boundary arr ix
			
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
	=> Stencil DIM2 a b
	-> Boundary a
	-> Array DIM2 a
	-> DIM2 -> b

{-# INLINE unsafeAppStencilBorder2 #-}
unsafeAppStencilBorder2
	 stencil@(StencilStatic  sExtent zero load)
	boundary@(BoundConst bZero)
	     arr@(Manifest aExtent vec)
	ix@(Z :. y :. x)
	
	| _ :. sHeight :. sWidth	<- sExtent
	, _ :. aHeight :. aWidth	<- aExtent
	, sHeight <= 5, sWidth <= 5
	= let
		{-# INLINE outside #-}
		outside xx yy
		 | yy < 0	 = True
		 | yy >= aHeight = True
		 | xx < 0	 = True
		 | xx >= aWidth	 = True
		 | otherwise	 = False
	
		{-# INLINE oload #-}
		oload oy ox
	 	 = let	!yy 	 = y + oy
			!xx 	 = x + ox
			!ix'	 = Z :. oy :. ox
	
		   in	if outside xx yy 
			 	then load ix' bZero
				else load ix' (arr `unsafeIndex` (Z :. yy :. xx))

	  in	template5x5 oload zero
	
	| otherwise
	= error "unsafeAppStencil2: finish this for larger stencils"


-- | Data template for stencils up to 5x5.
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

