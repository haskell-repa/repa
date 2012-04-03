{-# LANGUAGE BangPatterns, PackageImports #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Generic stencil based convolutions. 
-- 
--   If your stencil fits within a 7x7 tile and is known at compile-time then using
--   then using the built-in stencil support provided by the main Repa package will
--   be 5-10x faster. 
-- 
--   If you have a larger stencil, the coefficients are not statically known, 
--   or need more complex boundary handling than provided by the built-in functions,
--   then use this version instead.
--
module Data.Array.Repa.Algorithms.Convolve
	( -- * Arbitrary boundary handling
          convolve

          -- * Specialised boundary handling
	, GetOut
	, outAs
	, outClamp
	, convolveOut )
where
import Data.Array.Repa 					as A
import Data.Array.Repa.Repr.Unboxed                     as A
import qualified Data.Vector.Unboxed			as V
import qualified Data.Array.Repa.Shape			as S
import Prelude						as P


-- Plain Convolve -------------------------------------------------------------
-- | Image-kernel convolution,
--   which takes a function specifying what value to return when the
--   kernel doesn't apply.
convolve
	:: (Num a, Unbox a)
	=> (DIM2 -> a) 		-- ^ Function to get border elements when 
                                --   the stencil does not apply.
	-> Array U DIM2 a	-- ^ Stencil to use in the convolution.
	-> Array U DIM2 a	-- ^ Input image.
	-> Array U DIM2 a

convolve makeOut kernel image
 = kernel `deepSeqArray` image `deepSeqArray` 
   computeP $ unsafeTraverse image id update
 where	
        (Z :. krnHeight :. krnWidth)        = extent kernel
        krnVec          = toUnboxed kernel
        
        imgSh@(Z :. imgHeight :. imgWidth)  = extent image
        imgVec          = toUnboxed image

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
{-# INLINE convolve #-}


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
	:: (Num a, Unbox a)
	=> GetOut a		-- ^ How to handle out-of-range elements.
	-> Array U DIM2 a	-- ^ Stencil to use in the convolution.
	-> Array U DIM2 a	-- ^ Input image.
	-> Array U DIM2 a

{-# INLINE convolveOut #-}
convolveOut getOut kernel image
 = kernel `deepSeqArray` image `deepSeqArray` 
   computeP $ unsafeTraverse image id stencil
 where	
        krnSh@(Z :. krnHeight :. krnWidth)  = extent kernel        
        imgSh@(Z :. imgHeight :. imgWidth)  = extent image

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

