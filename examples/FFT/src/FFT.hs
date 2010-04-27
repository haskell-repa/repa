{-# LANGUAGE TypeOperators #-}

-- | Computation of Fast Fourier Transforms using the Cooley-Tuckey algorithm.
module FFT 
	( fft
	, ifft
	, fftWithRoots )
where
import Data.Array.Repa		as A
import Data.Ratio
import StrictComplex
import Roots


-- | Compute the (fast) Discrete Fourier Transform of a vector.
fft	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

fft v
 = let	rofu	= calcRofu (extent v)
   in	force $ fftWithRoots rofu v


-- | Compute the (fast) Inverse Discrete Fourier Transform of a vector.
ifft	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

ifft v
 = let	_ :. len	= extent v
	scale		= fromIntegral len :*: 0
	rofu		= calcInverseRofu (extent v)
   in	force $ A.map (/ scale) $ fftWithRoots rofu v


-- | Generic function for computation of forward or inverse Discrete Fourier Transforms.
--	The length of the roots vector must be the same as the values vector.
--	The length of these vectors must be a power of two.
fftWithRoots	
	:: Shape sh
	=> Array (sh :. Int) Complex		-- ^ Roots of unity.
	-> Array (sh :. Int) Complex		-- ^ Input values.
        -> Array (sh :. Int) Complex

fftWithRoots rofu v
	| not $ (denominator $ toRational (logBase 2 $ fromIntegral vLen)) == 1
	= error $ "fft: vector length of " ++ show vLen ++ " is not a power of 2"
	
	| rLen /= vLen
	= error $ "fft: length of vector is not the length of the roots"
	
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
	_ :. 2	-> fft_two   rofu v
	dim	-> fft_split rofu v dim

{-# INLINE fft_two #-}
fft_two rofu v
 = let	vFn' vFn (sh :. 0)  = vFn (sh :. 0) + vFn (sh :. 1)
	vFn' vFn (sh :. 1)  = vFn (sh :. 0) - vFn (sh :. 1)
   in	traverse v id vFn'
	
{-# INLINE fft_split #-}
fft_split rofu v vLen
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
   in	traverse v
		(\(vSh :. vLen)    -> vSh :. 2 :. (vLen `div` 2)) 
		vFn'
        
