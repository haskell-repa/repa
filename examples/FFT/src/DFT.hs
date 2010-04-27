{-# LANGUAGE TypeOperators #-}

module DFT 
	( dft
	, idft
	, dftWithRoots)
where
import Data.Array.Repa		as A
import Data.Ratio
import StrictComplex
import Roots

-- | Compute the (non-fast) Discrete Fourier Transform of a vector.
dft 	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

dft v
 = let	rofu	= calcRofu (extent v)
   in	force $ dftWithRoots rofu v


-- | Compute the inverse (non-fast) Discrete Fourier Transform of a vector.
idft 	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

idft v
 = let	_ :. len	= extent v
	scale		= fromIntegral len :*: 0
	rofu		= calcInverseRofu (extent v)
   in	force $ A.map (/ scale) $ dftWithRoots rofu v


-- | Generic function for computation of forward or inverse Discrete Fourier Transforms.
dftWithRoots
	:: Shape sh
	=> Array (sh :. Int) Complex		-- ^ Roots of unity for this vector length.
	-> Array (sh :. Int) Complex		-- ^ Input vector.
	-> Array (sh :. Int) Complex

dftWithRoots rofu arr
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

