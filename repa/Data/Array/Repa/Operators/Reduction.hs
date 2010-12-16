{-# LANGUAGE ExplicitForAll, TypeOperators #-}

module Data.Array.Repa.Operators.Reduction
	( fold, foldAll
	, sum,  sumAll)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Shape		as S
import qualified Data.Vector.Unboxed	as V
import Prelude				hiding (sum)


-- | Sequentially fold the innermost dimension of an array.
--	Combine this with `transpose` to fold any other dimension.
fold 	:: (Shape sh, Elt a)
	=> (a -> a -> a)
	-> a 
	-> Array (sh :. Int) a
	-> Array sh a

{-# INLINE fold #-}
fold f x arr
 = x `seq` arr `deepSeqArray` 
   let	sh' :. n	= extent arr
	elemFn i 	= V.foldl' f x
			$ V.map	(\ix -> arr ! (i :. ix)) 
				(V.enumFromTo 0 (n - 1))
   in	Delayed sh' elemFn


-- | Sequentially fold all the elements of an array.
foldAll :: (Shape sh, Elt a)
	=> (a -> a -> a)
	-> a
	-> Array sh a
	-> a
	
{-# INLINE foldAll #-}
foldAll f x arr
	= V.foldl' f x
	$ V.map ((arr !) . (S.fromIndex (extent arr)))
	$ V.enumFromTo
		0
		((S.size $ extent arr) - 1)



-- | Sum the innermost dimension of an array.
sum	:: (Shape sh, Elt a, Num a)
	=> Array (sh :. Int) a
	-> Array sh a

{-# INLINE sum #-}
sum arr	= fold (+) 0 arr


-- | Sum all the elements of an array.
sumAll	:: (Shape sh, Elt a, Num a)
	=> Array sh a
	-> a

{-# INLINE sumAll #-}
sumAll arr
	= V.foldl' (+) 0
	$ V.map ((arr !) . (S.fromIndex (extent arr)))
	$ V.enumFromTo
		0
		((S.size $ extent arr) - 1)

