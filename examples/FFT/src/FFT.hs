{-# LANGUAGE TypeOperators #-}

module FFT (fft) where
import Data.Array.Repa
import Data.Ratio
import StrictComplex


-- | Compute the FFT of a vector
fft	:: Shape sh
	=> Array (sh :. Int) Complex		-- ^ Roots of unity.
	-> Array (sh :. Int) Complex		-- ^ Input values.
        -> Array (sh :. Int) Complex

fft rofu v
	| not $ (denominator $ toRational (logBase 2 $ fromIntegral vLen)) == 1
	= error $ "fft: vector length of " ++ show vLen ++ " is not a power of 2"
	
	| rLen /= vLen
	= error $ "fft: length of vector is not the length of the roots"
	
	| otherwise
	= fft' rofu v

	where	_ :. rLen	= extent rofu
		_ :. vLen	= extent v


fft'	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
        -> Array (sh :. Int) Complex

{-# INLINE fft' #-}
fft' rofu v
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
 = let 	fft_lr = force $ fft' (splitRofu rofu) (splitVector v)

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
        
