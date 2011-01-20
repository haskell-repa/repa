{-# LANGUAGE MagicHash, PatternGuards, BangPatterns  #-}
{-# OPTIONS -Wnot #-}

-- | Class of types that can be used as array shapes and indices.
module Data.Array.Repa.Stencil
	( Stencil	(..)
	, Boundary	(..)
	, makeConvolution
	, mapStencil2)
where
import Data.Array.Repa			as R
import qualified Data.Array.Repa.Shape	as S
import qualified Data.Vector.Unboxed	as V
import GHC.Exts

-- | Represents a stencil we can apply to an array.
--   TODO: Change to StencilStatic.
--	   Add       StencilDense and StencilSparse.
--
data Stencil sh a b
	= Stencil
	{ stencilExtent	:: !sh
	, stencilZero	:: !b 
	, stencilAcc	:: !(sh -> a -> b -> b) }

data Boundary a
	= BoundConst a


-- | Make a convolution stencil.
makeConvolution
	:: (Elt a, Num a) 
	=> sh			-- zero value of output
	-> (sh -> Maybe a) 	-- fn to get coefficients for each value of the stencil.
	-> Stencil sh a a

{-# INLINE makeConvolution #-}
makeConvolution ex getCoeff
 = Stencil ex 0 
 $ \ix val acc
	-> case getCoeff ix of
		Nothing		-> acc
		Just coeff	-> acc + val * coeff
	

-- Apply a stencil to every element of an array
--   This is specialised for 3x3 and 5x5 stencils. 
--   Smaller kernels that fit into one of the specialised sizes are still specialised (like 3x2)
mapStencil2 
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b
	-> Boundary a
	-> Array DIM2 a -> Array DIM2 b

{-# INLINE mapStencil2 #-}
mapStencil2 stencil@(Stencil sExtent zero load) boundary arr
 = let	(_ :. aHeight :. aWidth) = extent arr
	(_ :. sHeight :. sWidth) = sExtent

	sHeight2	= sHeight `div` 2
	sWidth2		= sWidth  `div` 2

	xMin		= sWidth2  + 1
	yMin		= sHeight2 + 1
	xMax		= aWidth  - sWidth2  - 1
	yMax		= aHeight - sHeight2 - 1
	
	rngInternal	= ( Z :. yMin :. xMin
			  , Z :. yMax :. xMax )
			
	{-# INLINE getInner_mapStencil2 #-}	
	getInner_mapStencil2 ix	
			= unsafeAppStencilInternal2 stencil arr ix
						
   in	Partitioned 
		(extent arr)
		(const False)
		[]		(unsafeAppStencilBorder2   stencil boundary arr)
		rngInternal	getInner_mapStencil2
		

-- | Apply a stenil to a single position in an image.
--	This version can be applied close to the border, or even outside the image.
unsafeAppStencilBorder2
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b
	-> Boundary a
	-> Array DIM2 a
	-> DIM2 -> b

{-# INLINE unsafeAppStencilBorder2 #-}
unsafeAppStencilBorder2
	 stencil@(Stencil  sExtent zero load)
	boundary@(BoundConst bZero)
	arr ix@(Z :. y :. x)
 | height <= 5
 , width  <= 5
 = 	  oload (-2) (-2)  $ oload (-2) (-1)  $  oload (-2)   0  $  oload (-2)   1  $  oload (-2)   2 
	$ oload (-1) (-2)  $ oload (-1) (-1)  $  oload (-1)   0  $  oload (-1)   1  $  oload (-1)   2 
	$ oload   0  (-2)  $ oload   0  (-1)  $  oload   0    0  $  oload   0    1  $  oload   0    2  
	$ oload   1  (-2)  $ oload   1  (-1)  $  oload   1    0  $  oload   1    1  $  oload   1    2 
	$ oload   2  (-2)  $ oload   2  (-1)  $  oload   2    0  $  oload   2    1  $  oload   2    2 
	$ zero

 | otherwise
 = error "unsafeAppStencil2: finish this for larger stencils"

 where	_ :. height :. width	= sExtent
	
	{-# INLINE oload #-}
	oload ox oy
	 = let 	!yy 	= y + oy
		!xx 	= x + ox
		!ix'	= Z :. oy :. ox
		
		result
		 | yy < 0	= load ix' bZero
		 | yy >= height	= load ix' bZero
		 | xx < 0	= load ix' bZero
		 | xx >= width	= load ix' bZero
		 | otherwise	= load ix' (arr `unsafeIndex` (Z :. yy :. xx))
		
	   in	result


-- | Apply a stencil to a single, internal position in an image.
--	Applying it too close to the border yields badness.
--	TODO: fold the loading stuff into code for unsafeAppStencilBorder.
--	TODO: force delayed arrays before processing them.

unsafeAppStencilInternal2
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b -> Array DIM2 a -> DIM2 -> b

{-# INLINE unsafeAppStencilInternal2 #-}
unsafeAppStencilInternal2 
	stencil@(Stencil sExtent zero load)
	    arr@(Manifest (_ :. height :. width) vec) 
	     ix@(Z :. y :. x)
 = case sExtent of
    Z :. height :. width
       | height <= 5
       , width  <= 5
       -> oload (-2) (-2)  $ oload (-2) (-1)  $  oload (-2)   0  $  oload (-2)   1  $  oload (-2)   2 
	$ oload (-1) (-2)  $ oload (-1) (-1)  $  oload (-1)   0  $  oload (-1)   1  $  oload (-1)   2 
	$ oload   0  (-2)  $ oload   0  (-1)  $  oload   0    0  $  oload   0    1  $  oload   0    2  
	$ oload   1  (-2)  $ oload   1  (-1)  $  oload   1    0  $  oload   1    1  $  oload   1    2 
	$ oload   2  (-2)  $ oload   2  (-1)  $  oload   2    0  $  oload   2    1  $  oload   2    2 
	$ zero

    _ -> error "unsafeAppStencil2: finish this for larger stencils"

 where	-- We want to go access the vector directly here so we don't 
	-- have to calculate incides for every access of the source array.
	!center	= x + y * width
	
	{-# INLINE oload #-}
	oload oy ox	
	 = load (Z :. oy :. ox) (vec `V.unsafeIndex` (center + ox + oy * width))
			
