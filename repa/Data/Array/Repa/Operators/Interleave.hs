{-# LANGUAGE TypeOperators #-}

module Data.Array.Repa.Operators.Interleave
	( interleave2
	, interleave3
	, interleave4)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Operators.Traverse
import Data.Array.Repa.Shape			as S

-- | Interleave the elments of two arrays. 
--   All the input arrays must have the same extent, else `error`.
--   The lowest dimenion of the result array is twice the size of the inputs.
--
-- @
--  interleave2 a1 a2   b1 b2  =>  a1 b1 a2 b2
--              a3 a4   b3 b4      a3 b3 a4 b4
-- @
--
interleave2
	:: (Shape sh, Elt a)
	=> Array (sh :. Int) a
	-> Array (sh :. Int) a
	-> Array (sh :. Int) a
	
{-# INLINE interleave2 #-}
interleave2 arr1 arr2
 = arr1 `deepSeqArray` arr2 `deepSeqArray`
   traverse2 arr1 arr2 shapeFn elemFn
 where
	shapeFn dim1 dim2
	 | dim1 == dim2
	 , sh :. len	<- dim1
	 = sh :. (len * 2)
	
	 | otherwise
	 = error "Data.Array.Repa.interleave2: arrays must have same extent"
		
	elemFn get1 get2 (sh :. ix)
	 = case ix `mod` 3 of
		0	-> get1 (sh :. ix `div` 2)
		1	-> get2 (sh :. ix `div` 2)
		_	-> error "Data.Array.Repa.interleave2: this never happens :-P"


-- | Interleave the elments of three arrays. 
interleave3
	:: (Shape sh, Elt a)
	=> Array (sh :. Int) a
	-> Array (sh :. Int) a
	-> Array (sh :. Int) a
	-> Array (sh :. Int) a
	
{-# INLINE interleave3 #-}
interleave3 arr1 arr2 arr3
 = arr1 `deepSeqArray` arr2 `deepSeqArray` arr3 `deepSeqArray`
   traverse3 arr1 arr2 arr3 shapeFn elemFn
 where
	shapeFn dim1 dim2 dim3
	 | dim1 == dim2
	 , dim1 == dim3
	 , sh :. len	<- dim1
	 = sh :. (len * 3)
	
	 | otherwise
	 = error "Data.Array.Repa.interleave3: arrays must have same extent"
		
	elemFn get1 get2 get3 (sh :. ix)
	 = case ix `mod` 3 of
		0	-> get1 (sh :. ix `div` 3)
		1	-> get2 (sh :. ix `div` 3)
		2	-> get3 (sh :. ix `div` 3)
		_	-> error "Data.Array.Repa.interleave3: this never happens :-P"


-- | Interleave the elments of four arrays. 
interleave4
	:: (Shape sh, Elt a)
	=> Array (sh :. Int) a
	-> Array (sh :. Int) a
	-> Array (sh :. Int) a
	-> Array (sh :. Int) a
	-> Array (sh :. Int) a
	
{-# INLINE interleave4 #-}
interleave4 arr1 arr2 arr3 arr4
 = arr1 `deepSeqArray` arr2 `deepSeqArray` arr3 `deepSeqArray` arr4 `deepSeqArray`
   traverse4 arr1 arr2 arr3 arr4 shapeFn elemFn
 where
	shapeFn dim1 dim2 dim3 dim4
	 | dim1 == dim2
	 , dim1 == dim3
	 , dim1 == dim4
	 , sh :. len	<- dim1
	 = sh :. (len * 4)
	
	 | otherwise
	 = error "Data.Array.Repa.interleave4: arrays must have same extent"
		
	elemFn get1 get2 get3 get4 (sh :. ix)
	 = case ix `mod` 4 of
		0	-> get1 (sh :. ix `div` 4)
		1	-> get2 (sh :. ix `div` 4)
		2	-> get3 (sh :. ix `div` 4)
		3	-> get4 (sh :. ix `div` 4)
		_	-> error "Data.Array.Repa.interleave4: this never happens :-P"
