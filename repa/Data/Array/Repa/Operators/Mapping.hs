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
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Base
import Data.Array.Repa.Operators.Traverse
import Data.Array.Repa.Shape		as S
import Prelude				hiding (map, zipWith)


-- | Apply Partitioned worker function to each element of an array, 
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
	 -> let {-# INLINE getElem' #-}
	    	getElem'	= f . getElem
	    in	Delayed sh getElem'

	DelayedCursor sh makeCursor shiftCursor loadElem
	 -> let {-# INLINE loadElem' #-}
		loadElem'	= f . loadElem
	    in	DelayedCursor sh makeCursor shiftCursor loadElem'

	Partitioned sh inBorder
		rngsBorder getBorder
		rngInner   getInner
		
	 -> let	{-# INLINE getBorder' #-}
		getBorder' 	= f . getBorder

		{-# INLINE getInner' #-}
		getInner' 	= f . getInner

	    in	Partitioned sh inBorder
			rngsBorder getBorder'
			rngInner   getInner'


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
 	| not $ isPartitioned arr1
 	, not $ isPartitioned arr2
 	= let	{-# INLINE getElem' #-}
		getElem' ix	= f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix)
	  in	Delayed	(S.intersectDim (extent arr1) (extent arr2))
			getElem'

	-- TODO: intersect array ranges
	| not $ isPartitioned arr1
	, Partitioned sh inBorder rngsBorder fnBorder rngInner fnInner <- arr2
	= let	{-# INLINE getBorder'  #-}
		getBorder' ix = f (arr1 `unsafeIndex` ix) (fnBorder ix)
		
		{-# INLINE getInner' #-}
		getInner'  ix = f (arr1 `unsafeIndex` ix) (fnInner  ix)

	  in 	Partitioned sh inBorder
			rngsBorder getBorder'
			rngInner   getInner'

	-- TODO: intersect array ranges
	| Partitioned sh inBorder rngsBorder fnBorder rngInner fnInner <- arr1
	, not $ isPartitioned arr2
	= let	{-# INLINE getElemBorder #-}
		getElemBorder ix = f (fnBorder ix) (arr2 `unsafeIndex` ix)

		{-# INLINE getElemInner #-}
		getElemInner  ix = f (fnInner  ix) (arr2 `unsafeIndex` ix)

	  in	Partitioned sh inBorder
			rngsBorder getElemBorder
			rngInner   getElemInner
	
	-- TODO: convert to flattened form before zip/
	--       or we could be tricky and clip the ranges against each other.
	| Partitioned{} <- arr1
	, Partitioned{} <- arr2
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
