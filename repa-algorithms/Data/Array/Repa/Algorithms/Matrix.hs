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

-- | Matrix-matrix multiply.
multiplyMM
	:: Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

{-# NOINLINE multiplyMM #-}
multiplyMM arr@(Array _ [Region RangeAll (GenManifest _)])
	   brr@(Array _ [Region RangeAll (GenManifest _)])
 = [arr, brr] `deepSeqArrays`
   A.force $ A.sum (A.zipWith (*) arrRepl brrRepl)
 where	trr@(Array _ [Region RangeAll (GenManifest _)])
			= force $ transpose2D brr
	arrRepl		= trr `deepSeqArray` A.extend (Z :. All   :. colsB :. All) arr
	brrRepl		= trr `deepSeqArray` A.extend (Z :. rowsA :. All   :. All) trr
	(Z :. _     :. rowsA) = extent arr
	(Z :. colsB :. _    ) = extent brr
	

transpose2D :: Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE transpose2D #-}
transpose2D arr
 = backpermute new_extent swap arr
 where	swap (Z :. i :. j)	= Z :. j :. i
	new_extent		= swap (extent arr)
