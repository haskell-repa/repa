
module SolveDArray
	( wrapDArraySolver
	, mmMult_replicate
	, mmMult_traverse)
where
import DArray				as DA
import Array				as A

{-# INLINE wrapDArraySolver #-}
wrapDArraySolver solver mat1 mat2
	= fromDArray (solver (toDArray mat1) (toDArray mat2))


mmMult_replicate	
	:: DArray DIM2 Double 
	-> DArray DIM2 Double 
	-> DArray DIM2 Double  

mmMult_replicate 
	arr1@(DArray (sh :. m1 :. n1) fn1) 
	arr2@(DArray (sh' :. m2 :. n2) fn2) 
 = fold (+) 0 (arr1Ext * arr2Ext)
 where
    arr2T   = forceDArray $ transpose arr2
    arr1Ext = replicateSlice arr1  ((A.Any sh) :. A.All :. m2 :. A.All)
    arr2Ext = replicateSlice arr2T ((A.Any sh) :. n1 :. A.All :. A.All)


mmMult_traverse
	:: DArray DIM2 Double 
	-> DArray DIM2 Double 
	-> DArray DIM2 Double  

mmMult_traverse arr1 arr2 
 = fold (+) 0 arr'
 where 
    arrT = forceDArray $ transpose arr2
    arr' = traverse2DArray arr1 arrT 
      (\(sh :. m1 :. n1) -> \(_ :. n2 :. m2) -> (sh :. m1 :. n2 :. n1))
      (\f1 -> \f2 -> \(sh :. i :. j :. k) -> f1 (sh :. i :. k) * f2 (sh :. j :. k))
