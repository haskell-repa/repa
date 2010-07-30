{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE PackageImports #-}

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
import Data.Array.Repa	as A
import "dph-prim-par" Data.Array.Parallel.Unlifted			(Elt)

-- | Matrix-matrix multiply.
multiplyMM
	:: (Num a, Elt a)
	=> Array DIM2 a
	-> Array DIM2 a
	-> Array DIM2 a

-- NOTE: We need this INLINE so multiplyMM gets specialised for the appropriate types.
{-# INLINE multiplyMM #-}
multiplyMM arr1 arr2
 = multiplyMM' (force arr1) (force arr2)

 where multiplyMM' arr@Manifest{} brr@Manifest{}
	= A.force $ A.sum (A.zipWith (*) arrRepl brrRepl)
	where
	 trr		= force $ transpose2D brr
	 arrRepl	= A.replicate (Z :. All   :. colsB :. All) arr
	 brrRepl	= A.replicate (Z :. rowsA :. All   :. All) trr
	 (Z :. _     :. rowsA) = extent arr
	 (Z :. colsB :. _    ) = extent brr
	

transpose2D :: Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE transpose2D #-}
transpose2D arr
 = backpermute new_extent swap arr
 where
	swap (Z :. i :. j)	= Z :. j :. i
	new_extent		= swap (extent arr)

