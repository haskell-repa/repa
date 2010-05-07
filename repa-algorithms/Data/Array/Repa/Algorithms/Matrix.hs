{-# OPTIONS -fno-warn-incomplete-patterns #-}

-- | Algorithms operating on matrices.
-- 
--   These functions should give performance comparable with nested loop C
--   implementations, but not block-based, cache friendly, SIMD using, vendor
--   optimised implementions. 
--   If you care deeply about runtime performance then you may be better off using 
--   a binding to LAPACK, such as hvector.
--
module Data.Array.Repa.Algorithms.Matrix
	(multiplyMM)
where
import Data.Array.Repa
	

-- | Matrix-matrix multiply.
--
multiplyMM	
	:: Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

multiplyMM  arr1 arr2
 = multiplyMM' (force arr1) (force arr2)
 where multiplyMM' arr1'@Manifest{} arr2'@Manifest{}
	= fold (+) 0 
  	$ traverse2 arr1' (force $ transpose arr2')
      		(\(sh :. m1 :. n1) -> \(_ :. n2 :. _m2) -> (sh :. m1 :. n2 :. n1))
		(\f1 -> \f2 -> \(sh :. i :. j :. k) -> f1 (sh :. i :. k) * f2 (sh :. j :. k))
