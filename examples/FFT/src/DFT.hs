{-# LANGUAGE TypeOperators #-}

module DFT (dft) where
import Data.Array.Repa		as A
import Data.Ratio
import StrictComplex


-- | Compute the (non-fast) Discrete Fourier Transform of a vector.
dft	:: Shape sh
	=> Array (sh :. Int) Complex		-- ^ Roots of unity.
	-> Array (sh :. Int) Complex		-- ^ Input values.
        -> Array (sh :. Int) Complex

dft rofu v
	| not $ (denominator $ toRational (logBase 2 $ fromIntegral vLen)) == 1
	= error $ "fft: vector length of " ++ show vLen ++ " is not a power of 2"
	
	| rLen /= vLen
	= error $ "fft: length of vector is not the length of the roots"
	
	| otherwise
	= dft' rofu v

	where	_ :. rLen	= extent rofu
		_ :. vLen	= extent v


dft'	:: Shape sh
	=> Array (sh :. Int) Complex		-- ^ Roots of unity for this vector length.
	-> Array (sh :. Int) Complex		-- ^ Input vector.
	-> Array (sh :. Int) Complex

dft' rofu arr
 = traverse arr id (\_ k -> dftK rofu arr k)
 

-- | Compute one value of the DFT.
dftK	:: Shape sh
	=> Array (sh :. Int) Complex 		-- ^ Roots of unity for this vector length.
	-> Array (sh :. Int) Complex		-- ^ Input vector.
	-> (sh :. Int)				-- ^ Index of the value we want.
	-> Complex

dftK rofu arrX (_ :. k)
 = A.sumAll $ A.zipWith (*) arrX wroots
 where	sh@(_ :. len)	= extent arrX

	-- All the roots we need to multiply with.
	wroots		= fromFunction sh elemFn
	elemFn (sh :. n) 
		= rofu !: (sh :. (k * n) `mod` len)

