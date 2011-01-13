{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction, PatternGuards #-}

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
 = case arr of
	Manifest{}
	 -> unsafeTraverse arr id (f .)

	Delayed sh getElem
	 -> Delayed sh (f . getElem)

	Segmented sh inBorder
		rngsBorder getBorder
		rngInner   getInner
	 -> Segmented sh inBorder
		rngsBorder (f . getBorder)
		rngInner   (f . getInner)


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
 	| not $ isSegmented arr1
 	, not $ isSegmented arr2
 	= Delayed
		(S.intersectDim (extent arr1) (extent arr2))
		(\ix -> f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix))

	-- TODO: intersect array ranges
	| not $ isSegmented arr1
	, Segmented sh inBorder rngsBorder fnBorder rngInner fnInner <- arr2
	= Segmented sh inBorder
		rngsBorder (\ix -> f (arr1 `unsafeIndex` ix) (fnBorder ix))
		rngInner   (\ix -> f (arr1 `unsafeIndex` ix) (fnInner  ix))

	-- TODO: intersect array ranges
	| Segmented sh inBorder rngsBorder fnBorder rngInner fnInner <- arr1
	, not $ isSegmented arr2
	= Segmented sh inBorder
		rngsBorder (\ix -> f (fnBorder ix) (arr2 `unsafeIndex` ix))
		rngInner   (\ix -> f (fnInner  ix) (arr2 `unsafeIndex` ix))
	
	-- TODO: convert to flattened form before zip/
	--       or we could be tricky and clip the ranges against each other.
	| Segmented{} <- arr1
	, Segmented{} <- arr2
	= error "finish this"

	| otherwise
	= error "zipWith: this doesn't happen :-P"


{-# INLINE (+^) #-}
(+^)	= zipWith (+)

{-# INLINE (-^) #-}
(-^)	= zipWith (-)

{-# INLINE (*^) #-}
(*^)	= zipWith (*)

{-# INLINE (/^) #-}
(/^)	= zipWith (/)
