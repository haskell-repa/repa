{-# LANGUAGE TypeOperators, RankNTypes, PatternGuards #-}

-- | Compute the Discrete Fourier Transform (DFT) along the low order dimension
--   of an array. 
--
--   This uses the naive algorithm and takes O(n^2) time. 
--   However, you can transform an array with an arbitray extent, unlike with FFT which requires
--   each dimension to be a power of two.
--
--   The `dft` and `idft` functions also compute the roots of unity needed.
--   If you need to transform several arrays with the same extent then it is faster to
--   compute the roots once using `calcRootsOfUnity` or `calcInverseRootsOfUnity`, 
--   then call `dftWithRoots` directly.
--
--   You can also compute single values of the transform using `dftWithRootsSingle`.
module Data.Array.Repa.Algorithms.DFT 
	( dft
	, idft
	, dftWithRoots
	, dftWithRootsSingle)
where
import Data.Array.Repa.Algorithms.DFT.Roots
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa				as A
import Prelude					as P

-- | Compute the DFT along the low order dimension of an array.
dft 	:: forall sh
	.  Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

dft v
 = let	rofu	= calcRootsOfUnity (extent v)
   in	force $ dftWithRoots rofu v


-- | Compute the inverse DFT along the low order dimension of an array.
idft 	:: forall sh
	.  Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

idft v
 = let	_ :. len	= extent v
	scale		= (fromIntegral len, 0)
	rofu		= calcInverseRootsOfUnity (extent v)
   in	force $ A.map (/ scale) $ dftWithRoots rofu v


-- | Generic function for computation of forward or inverse DFT.
--	This function is also useful if you transform many arrays with the same extent, 
--	and don't want to recompute the roots for each one.
--	The extent of the given roots must match that of the input array, else `error`.
dftWithRoots
	:: forall sh
	.  Shape sh
	=> Array (sh :. Int) Complex		-- ^ Roots of unity.
	-> Array (sh :. Int) Complex		-- ^ Input array.
	-> Array (sh :. Int) Complex

dftWithRoots rofu arr
	| _ :. rLen 	<- extent rofu
	, _ :. vLen 	<- extent arr
	, rLen /= vLen
	= error $    "dftWithRoots: length of vector (" P.++ show vLen P.++ ")"
		P.++ " does not match the length of the roots (" P.++ show rLen P.++ ")"

	| otherwise
	= traverse arr id (\_ k -> dftWithRootsSingle rofu arr k)
		

-- | Compute a single value of the DFT.
--	The extent of the given roots must match that of the input array, else `error`.
dftWithRootsSingle
	:: forall sh
	.  Shape sh
	=> Array (sh :. Int) Complex 		-- ^ Roots of unity.
	-> Array (sh :. Int) Complex		-- ^ Input array.
	-> (sh :. Int)				-- ^ Index of the value we want.
	-> Complex

{-# INLINE dftWithRootsSingle #-}
dftWithRootsSingle rofu arrX (_ :. k)
	| _ :. rLen 	<- extent rofu
	, _ :. vLen 	<- extent arrX
	, rLen /= vLen
	= error $    "dftWithRootsSingle: length of vector (" P.++ show vLen P.++ ")"
		P.++ " does not match the length of the roots (" P.++ show rLen P.++ ")"

	| otherwise
	= let	sh@(_ :. len)	= extent arrX

		-- All the roots we need to multiply with.
		wroots		= fromFunction sh elemFn
		elemFn (sh' :. n) 
			= rofu ! (sh' :. (k * n) `mod` len)

	  in  A.sumAll $ A.zipWith (*) arrX wroots


