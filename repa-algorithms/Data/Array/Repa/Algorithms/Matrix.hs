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
import Data.Array.Repa	                as A
import System.IO.Unsafe
import Data.Vector.Unboxed              (Unbox)

-- | Matrix-matrix multiply.
multiplyMM
	:: Array U DIM2 Double
	-> Array U DIM2 Double
	-> Array U DIM3 Double

{-# NOINLINE multiplyMM #-}
multiplyMM arr brr
 = unsafePerformIO 
 $ do   trr             <- now $ transpose2D brr
        let (Z :. rowsA :. _)     = extent arr
        let (Z :. _     :. colsB) = extent brr
        let arrRepl     = A.extend (Z :. All   :. colsB :. All) arr
        let brrRepl     = A.extend (Z :. rowsA :. All   :. All) trr
        return $ computeS $ A.zipWith (*) arrRepl brrRepl



{-
        [arr, brr] `deepSeqArrays`
   A.sumP (A.zipWith (*) arrRepl brrRepl)
 where	trr             = computeUnboxedP $ transpose2D brr
	arrRepl		= trr `deepSeqArray` A.extend (Z :. All   :. colsB :. All) arr
	brrRepl		= trr `deepSeqArray` A.extend (Z :. rowsA :. All   :. All) trr
	(Z :. _     :. rowsA) = extent arr
	(Z :. colsB :. _    ) = extent brr
-}	

transpose2D :: Array U DIM2 Double -> Array U DIM2 Double
{-# INLINE transpose2D #-}
transpose2D arr
 = arr `deepSeqArray` computeUnboxedP 
 $ unsafeBackpermute new_extent swap arr
 where	swap (Z :. i :. j)	= Z :. j :. i
	new_extent		= swap (extent arr)
