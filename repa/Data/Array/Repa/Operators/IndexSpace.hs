{-# LANGUAGE TypeOperators, ExplicitForAll, FlexibleContexts #-}

module Data.Array.Repa.Operators.IndexSpace
	( reshape
	, append, (++)
	, transpose
	, extend
	, slice
        , extract
	, backpermute,         unsafeBackpermute
	, backpermuteDft,      unsafeBackpermuteDft)
where
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Shape		as S
import Prelude				hiding ((++))
import qualified Prelude		as P

stage	= "Data.Array.Repa.Operators.IndexSpace"

-- Index space transformations ------------------------------------------------
-- | Impose a new shape on the elements of an array.
--   The new extent must be the same size as the original, else `error`.
reshape	:: (Shape sh2, Shape sh1
           , Repr r1 e)
	=> sh2
	-> Array r1 sh1 e
	-> Array D  sh2 e

reshape sh2 arr
	| not $ S.size sh2 == S.size (extent arr)
	= error 
        $ stage P.++ ".reshape: reshaped array will not match size of the original"

reshape sh2 arr
        = fromFunction sh2 
        $ unsafeIndex arr . fromIndex (extent arr) . toIndex sh2
{-# INLINE [2] reshape #-}
 

-- | Append two arrays.
append, (++)
	:: ( Shape sh
	   , Repr r1 e, Repr r2 e)
	=> Array r1 (sh :. Int) e
	-> Array r2 (sh :. Int) e
	-> Array D  (sh :. Int) e

append arr1 arr2
 = unsafeTraverse2 arr1 arr2 fnExtent fnElem
 where
 	(_ :. n) 	= extent arr1

	fnExtent (sh :. i) (_  :. j)
		= sh :. (i + j)

	fnElem f1 f2 (sh :. i)
      		| i < n		= f1 (sh :. i)
  		| otherwise	= f2 (sh :. (i - n))
{-# INLINE [2] append #-}


(++) arr1 arr2 = append arr1 arr2
{-# INLINE (++) #-}


-- | Transpose the lowest two dimensions of an array.
--	Transposing an array twice yields the original.
transpose
	:: ( Shape sh
	   , Repr r e)
	=> Array r (sh :. Int :. Int) e
	-> Array D (sh :. Int :. Int) e

transpose arr
 = unsafeTraverse arr
	(\(sh :. m :. n) 	-> (sh :. n :.m))
	(\f -> \(sh :. i :. j) 	-> f (sh :. j :. i))
{-# INLINE [2] transpose #-}


-- | Extend an array, according to a given slice specification.
extend
	:: ( Slice sl
	   , Shape (FullShape sl)
	   , Shape (SliceShape sl)
	   , Repr r e)
	=> sl
	-> Array r (SliceShape sl) e
	-> Array D (FullShape sl)  e

extend sl arr
	= unsafeBackpermute
		(fullOfSlice sl (extent arr))
		(sliceOfFull sl)
		arr
{-# INLINE [2] extend #-}


-- | Take a slice from an array, according to a given specification.
slice	:: ( Slice sl
	   , Shape (FullShape sl)
	   , Shape (SliceShape sl)
	   , Repr r e)
	=> Array r (FullShape sl) e
	-> sl
	-> Array D (SliceShape sl) e

slice arr sl
	= unsafeBackpermute
		(sliceOfFull sl (extent arr))
		(fullOfSlice sl)
		arr
{-# INLINE [2] slice #-}


-- | Extract a sub-range of elements from an array.
extract :: (Shape sh, Repr r e)
        => sh                   -- ^ Starting index.
        -> sh                   -- ^ Size of result.
        -> Array r sh e 
        -> Array D sh e
extract start sz arr
        = fromFunction sz (\ix -> arr `unsafeIndex` (addDim start ix))
{-# INLINE [2] extract #-}


-- | Backwards permutation of an array's elements.
--	The result array has the same extent as the original.
backpermute, unsafeBackpermute
	:: forall r sh1 sh2 e
	.  ( Shape sh1, Shape sh2
	   , Repr r e)
	=> sh2 			-- ^ Extent of result array.
	-> (sh2 -> sh1) 	-- ^ Function mapping each index in the result array
				--	to an index of the source array.
	-> Array r  sh1 e 	-- ^ Source array.
	-> Array D  sh2 e

backpermute newExtent perm arr
	= traverse arr (const newExtent) (. perm)
{-# INLINE [2] backpermute #-}

unsafeBackpermute newExtent perm arr
        = unsafeTraverse arr (const newExtent) (. perm)
{-# INLINE [2] unsafeBackpermute #-}


-- | Default backwards permutation of an array's elements.
--	If the function returns `Nothing` then the value at that index is taken
--	from the default array (@arrDft@)
backpermuteDft, unsafeBackpermuteDft
	:: forall r0 r1 sh1 sh2 e
	.  ( Shape sh1, Shape sh2
	   , Repr  r0 e, Repr r1 e)
	=> Array r0 sh2 e	-- ^ Default values (@arrDft@)
	-> (sh2 -> Maybe sh1) 	-- ^ Function mapping each index in the result array
				--	to an index in the source array.
	-> Array r1 sh1 e	-- ^ Source array.
	-> Array D  sh2 e

backpermuteDft arrDft fnIndex arrSrc
	= fromFunction (extent arrDft) fnElem
	where	fnElem ix
		 = case fnIndex ix of
			Just ix'	-> arrSrc `index` ix'
			Nothing		-> arrDft `index` ix
{-# INLINE [2] backpermuteDft #-}

unsafeBackpermuteDft arrDft fnIndex arrSrc
        = fromFunction (extent arrDft) fnElem
        where   fnElem ix
                 = case fnIndex ix of
                        Just ix'        -> arrSrc `unsafeIndex` ix'
                        Nothing         -> arrDft `unsafeIndex` ix
{-# INLINE [2] unsafeBackpermuteDft #-}

