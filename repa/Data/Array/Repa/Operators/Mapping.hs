{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Array.Repa.Operators.Mapping
	( map
	, zipWith
	, (+^)
	, (-^)
	, (*^)
	, (/^))
where
import Data.Array.Repa.Base
import Data.Array.Repa.Operators.Traverse
import Data.Array.Repa.Shape		as S
import Prelude				hiding (map, zipWith)


-- | Apply a worker function to each element of an array, 
--	yielding a new array with the same extent.
map	:: (Shape sh, Elt a, Elt b) 
	=> (a -> b)
	-> Array sh a
	-> Array sh b

{-# INLINE map #-}
map f arr
	= unsafeTraverse arr id (f .)
	

-- | Combine two arrays, element-wise, with a binary operator.
--	If the extent of the two array arguments differ, 
--	then the resulting array's extent is their intersection.
zipWith :: (Shape sh, Elt a, Elt b, Elt c) 
	=> (a -> b -> c) 
	-> Array sh a
	-> Array sh b
	-> Array sh c

{-# INLINE zipWith #-}
zipWith f arr1 arr2
 	= arr1 `deepSeqArray` 
	  arr2 `deepSeqArray`
	  Delayed	(S.intersectDim (extent arr1) (extent arr2))
			(\ix -> f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix))

{-# INLINE (+^) #-}
(+^)	= zipWith (+)

{-# INLINE (-^) #-}
(-^)	= zipWith (-)

{-# INLINE (*^) #-}
(*^)	= zipWith (*)

{-# INLINE (/^) #-}
(/^)	= zipWith (/)
