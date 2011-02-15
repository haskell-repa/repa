{-# LANGUAGE TypeOperators #-}

module Data.Array.Repa.Algorithms.FFTG
	(fftWithRoots')
where
import Data.Array.Repa.Algorithms.DFT.Roots
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa				as A
import Data.Ratio


-- | Compute the DFT along the low order dimension of an array.
fft1d	:: Array DIM1 Complex
	-> Array DIM1 Complex

fft1d v
 = let	rofu	= calcRootsOfUnity (extent v)
   in	force $ fftWithRoots' rofu v



fftWithRoots'
	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
        -> Array (sh :. Int) Complex

{-# INLINE fftWithRoots' #-}
fftWithRoots' rofu@Manifest{} v@Manifest{}
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
