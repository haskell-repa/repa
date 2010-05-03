{-# LANGUAGE TypeOperators, PatternGuards, RankNTypes #-}

-- | Fast computation of Discrete Fourier Transforms using the Cooley-Tuckey algorithm.
--
--   Time complexity is O(n log n) in the size of the input.
--
--   Input dimensions must be powers of two, else `error`.
--
--   The `fft` and `ifft` functions (and friends) also compute the roots of unity needed.
--   If you need to transform several arrays with the same extent then it is faster to
--   compute the roots once using `calcRootsOfUnity` or `calcInverseRootsOfUnity`, 
--   then call `fftWithRoots` directly.
--
--   The inverse transforms provided also perform post-scaling so that `ifft` is the true inverse of `fft`. 
--   If you don't want that then call `fftWithRoots` directly.
--
--   The functions `fft2d` and `fft3d` require their inputs to be squares (and cubes) respectively. 
--   This allows them to reuse the same roots-of-unity when transforming along each axis. If you 
--   need to transform rectanglular arrays then call `fftWithRoots` directly.
module Data.Array.Repa.Algorithms.FFT
	( fft,   ifft
	, fft2d, ifft2d
	, fft3d
	, fftWithRoots )
where
import Data.Array.Repa.Algorithms.DFT.Roots
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa				as A
import Data.Ratio

-- Vector Transform -------------------------------------------------------------------------------
-- | Compute the DFT along the low order dimension of an array.
fft	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

fft v
 = let	rofu	= calcRootsOfUnity (extent v)
   in	force $ fftWithRoots rofu v


-- | Compute the inverse DFT along the low order dimension of an array.
ifft	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

ifft v
 = let	_ :. len	= extent v
	scale		= fromIntegral len :*: 0
	rofu		= calcInverseRootsOfUnity (extent v)
   in	force $ A.map (/ scale) $ fftWithRoots rofu v


-- Matrix Transform -------------------------------------------------------------------------------
-- | Compute the DFT of a square matrix.
--   If the matrix is not square then `error`.
fft2d 	:: Array DIM2 Complex
	-> Array DIM2 Complex

fft2d arr
 	| Z :. height :. width	<- extent arr
 	, height /= width	
	= error $ "fft2d: height of matrix (" ++ show height ++ ")"
		++  " does not match width (" ++ show width  ++ ")"

	| otherwise
	= let	rofu		= calcRootsOfUnity (extent arr)
  		fftTrans 	= transpose . fftWithRoots rofu
   	  in	fftTrans $ fftTrans arr


-- | Compute the inverse DFT of a square matrix. 
ifft2d	:: Array DIM2 Complex
	-> Array DIM2 Complex
	
ifft2d arr
 	| Z :. height :. width	<- extent arr
 	, height /= width	
	= error $ "fft2d: height of matrix (" ++ show height ++ ")"
		++  " does not match width (" ++ show width  ++ ")"

	| otherwise
	= let	_ :. height :. width = extent arr
		scale		= fromIntegral (height * width) :*: 0
		rofu		= calcInverseRootsOfUnity (extent arr)
		fftTrans	= transpose . fftWithRoots rofu
	  in	force $ A.map (/ scale) $ fftTrans $ fftTrans arr
	

-- Cube Transform ---------------------------------------------------------------------------------
-- | Compute the DFT of a 3d cube.
--   If the array is not a cube then `error`.
fft3d 	:: Array DIM3 Complex
	-> Array DIM3 Complex

fft3d arrIn
 = let	rofu		= calcRootsOfUnity (extent arrIn)

	transpose3 arr
	 = traverse arr 
        	(\(Z :. k :. l :. m)   -> (Z :. l :. m :. k)) 
            	(\f (Z :. l :. m :. k) -> f (Z :. k :. l :. m)) 

	fftTrans	= transpose3 . fftWithRoots rofu
	
  in	fftTrans $ fftTrans $ fftTrans arrIn

	
-- Worker -----------------------------------------------------------------------------------------
-- | Generic function for computation of forward or inverse Discrete Fourier Transforms.
--	Computation is along the low order dimension of the array.
fftWithRoots	
	:: forall sh
	.  Shape sh
	=> Array (sh :. Int) Complex		-- ^ Roots of unity.
	-> Array (sh :. Int) Complex		-- ^ Input values.
        -> Array (sh :. Int) Complex

fftWithRoots rofu v
	| not $ (denominator $ toRational (logBase (2 :: Double) $ fromIntegral vLen)) == 1
	= error $ "fft: vector length of " ++ show vLen ++ " is not a power of 2"
	
	| rLen /= vLen
	= error $  "fft: length of vector (" ++ show vLen ++ ")"
		++ " does not match the length of the roots (" ++ show rLen ++ ")"
	
	| otherwise
	= fftWithRoots' rofu v

	where	_ :. rLen	= extent rofu
		_ :. vLen	= extent v

fftWithRoots'
	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
        -> Array (sh :. Int) Complex

{-# INLINE fftWithRoots' #-}
fftWithRoots' rofu v
 = case extent v of
	_ :. 2	-> fft_two   v
	_	-> fft_split rofu v

{-# INLINE fft_two #-}
fft_two v
 = let	vFn' vFn (sh :. 0)  = vFn (sh :. 0) + vFn (sh :. 1)
	vFn' vFn (sh :. 1)  = vFn (sh :. 0) - vFn (sh :. 1)
	vFn' _   _          = error "Data.Array.Repa.Algorithms.FFT fft_two fail"
   in	traverse v id vFn'
	
{-# INLINE fft_split #-}
fft_split rofu v
 = let 	fft_lr = force $ fftWithRoots' (splitRofu rofu) (splitVector v)

	fft_l  = traverse2 fft_lr rofu 
 		   (\(sh :. 2 :. n) _ -> sh :. n)
		   (\f r (sh :. i)    -> f (sh :. 0 :. i) + r (sh :. i) * f (sh :. 1 :. i))

	fft_r  = traverse2 fft_lr rofu 
		   (\(sh :. 2 :. n) _ -> sh :. n)
		   (\f r (sh :. i)    -> f (sh :. 0 :. i) - r (sh :. i) * f (sh :. 1 :. i))

   in	fft_l +:+ fft_r

{-# INLINE splitRofu #-}
splitRofu rofu
 = traverse rofu
	(\(rSh :. rLen) 	-> rSh :. (2::Int) :. (rLen `div` 2))
	(\rFn (sh :. _ :. i) 	-> rFn (sh :. 2*i))

{-# INLINE splitVector #-}
splitVector v 
 = let	vFn' vFn (sh :. 0 :. i) = vFn (sh :. 2*i)
	vFn' vFn (sh :. 1 :. i) = vFn (sh :. 2*i+1)
	vFn' _   _              = error "Data.Array.Repa.Algorithms.FFT splitVector fail"

   in	traverse v
		(\(vSh :. vLen)    -> vSh :. 2 :. (vLen `div` 2)) 
		vFn'
        
