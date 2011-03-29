{-# LANGUAGE BangPatterns, PackageImports #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Old support for stencil based convolutions. 
--
--   NOTE: This is slated to be merged with the new Stencil support in the next version
--         of Repa. We'll still expose the `convolve` function though.
--
module Data.Array.Repa.Algorithms.Convolve
	( convolve

	, GetOut
	, outAs
	, outClamp
	, convolveOut )
where
import Data.Array.Repa 					as A
import qualified Data.Vector.Unboxed			as V
import qualified Data.Array.Repa.Shape			as S
import Prelude						as P


-- Plain Convolve ---------------------------------------------------------------------------------
-- | Image-kernel convolution,
--   which takes a function specifying what value to return when the kernel doesn't apply.
convolve
	:: (Elt a, Num a)
	=> (DIM2 -> a) 		-- ^ Use this function to get border elements where the kernel apply.
	-> Array DIM2 a		-- ^ Kernel to use in the convolution.
	-> Array DIM2 a		-- ^ Input image.
	-> Array DIM2 a

{-# INLINE convolve #-}
convolve makeOut
 	kernel@(Array       (_ :. krnHeight :. krnWidth) [Region RangeAll (GenManifest krnVec)])
  	 image@(Array imgSh@(_ :. imgHeight :. imgWidth) [Region RangeAll (GenManifest imgVec)])

 = kernel `deepSeqArray` image `deepSeqArray` 
   force $ unsafeTraverse image id update
 where	
	!krnHeight2	= krnHeight `div` 2
	!krnWidth2	= krnWidth  `div` 2

	-- If we're too close to the edge of the input image then
	-- we can't apply the stencil because we don't have enough data.
	!borderLeft	= krnWidth2
	!borderRight	= imgWidth   - krnWidth2  - 1
	!borderUp	= krnHeight2
	!borderDown	= imgHeight  - krnHeight2 - 1

	{-# INLINE update #-}
	update _ ix@(_ :. j :. i)
 	 | i < borderLeft	= makeOut ix
 	 | i > borderRight	= makeOut ix
  	 | j < borderUp		= makeOut ix
 	 | j > borderDown	= makeOut ix
	 | otherwise		= stencil j i

	-- The actual stencil function.
	{-# INLINE stencil #-}
	stencil j i
	 = let	imgStart = S.toIndex imgSh (Z :. j - krnHeight2 :. i - krnWidth2)
	   in	integrate 0 0 0 imgStart 0

	{-# INLINE integrate #-}
	integrate !acc !x !y !imgCur !krnCur  
	 | y >= krnHeight
	 = acc

	 | x >= krnWidth
	 = integrate acc 0 (y + 1) (imgCur + imgWidth - krnWidth) krnCur 
	
	 | otherwise
	 = let	imgZ	= imgVec `V.unsafeIndex` imgCur 
		krnZ	= krnVec `V.unsafeIndex` krnCur 
		here	= imgZ * krnZ 
	   in	integrate (acc + here) (x + 1) y (imgCur + 1) (krnCur + 1)


-- Convolve Out -----------------------------------------------------------------------------------
-- | A function that gets out of range elements from an image.
type GetOut a
	= (DIM2 -> a) 	-- ^ The original get function.
	-> DIM2 	-- ^ The shape of the image.
	-> DIM2 	-- ^ Index of element we were trying to get.
	-> a


-- | Use the provided value for every out-of-range element.
outAs :: a -> GetOut a
{-# INLINE outAs #-}
outAs x _ _ _ = x


-- | If the requested element is out of range use
--   the closest one from the real image.
outClamp :: GetOut a
{-# INLINE outClamp #-}
outClamp get (_ :. yLen :. xLen) (sh :. j :. i)
 = clampX j i
 where 	{-# INLINE clampX #-}
	clampX !y !x
	  | x < 0	= clampY y 0
	  | x >= xLen	= clampY y (xLen - 1)
	  | otherwise	= clampY y x
		
	{-# INLINE clampY #-}
	clampY !y !x
	  | y < 0	= get (sh :. 0 		:. x)
	  | y >= yLen	= get (sh :. (yLen - 1) :. x)
	  | otherwise	= get (sh :. y 		:. x)


-- | Image-kernel convolution, 
--   which takes a function specifying what value to use for out-of-range elements.
convolveOut
	:: (Elt a, Num a)
	=> GetOut a		-- ^ Use this fn to get out of range elements.
	-> Array DIM2 a		-- ^ Kernel
	-> Array DIM2 a		-- ^ Image
	-> Array DIM2 a

{-# INLINE convolveOut #-}
convolveOut getOut
 	kernel@(Array krnSh@(_ :. krnHeight :. krnWidth) _)
  	 image@(Array imgSh@(_ :. imgHeight :. imgWidth) _)

 = kernel `deepSeqArray` image `deepSeqArray` 
   force $ unsafeTraverse image id stencil
 where	
	!krnHeight2	= krnHeight `div` 2
	!krnWidth2	= krnWidth  `div` 2
        !krnSize	= S.size krnSh

	-- If we're too close to the edge of the input image then
	-- we can't apply the stencil because we don't have enough data.
	!borderLeft	= krnWidth2
	!borderRight	= imgWidth   - krnWidth2  - 1
	!borderUp	= krnHeight2
	!borderDown	= imgHeight  - krnHeight2 - 1

	-- The actual stencil function.
	{-# INLINE stencil #-}
	stencil get (_ :. j :. i)
	 = let
		{-# INLINE get' #-}
		get' ix@(_ :. y :. x)
		 | x < borderLeft	= getOut get imgSh ix
		 | x > borderRight	= getOut get imgSh ix
		 | y < borderUp		= getOut get imgSh ix
		 | y > borderDown	= getOut get imgSh ix
		 | otherwise		= get ix

		!ikrnWidth'	= i - krnWidth2
		!jkrnHeight'	= j - krnHeight2

		{-# INLINE integrate #-}
		integrate !count !acc
		 | count == krnSize		= acc
		 | otherwise
		 = let	!ix@(sh :. y :. x)	= S.fromIndex krnSh count
			!ix'			= sh :. y + jkrnHeight' :. x + ikrnWidth'
			!here			= kernel `unsafeIndex` ix * (get' ix')
		   in	integrate (count + 1) (acc + here)

	   in	integrate 0 0

