{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Data.Array.Repa.Array
	( Array	(..)

	 -- * Basic Operations
	, force
	, (!:)
	
	 -- * Conversion
	, fromList
	, toList
	
	 -- * Computations
	, fold
	, zipWith
	, sumAll
	, tests_DataArrayRepaArray)
where
import Data.Array.Repa.Index
import Data.Array.Repa.QuickCheck
import Test.QuickCheck
import Data.Array.Repa.Shape			(Shape)
import Data.Array.Parallel.Unlifted		(Elt)
import qualified Data.Array.Repa.Shape		as S
import qualified Data.Array.Parallel.Unlifted	as U
import Prelude					as P hiding (zipWith)	

stage	= "Data.Array.Repa.Array"
	
	
-- | Possibly delayed arrays.
data Array sh e 
	= -- | An array represented as some concrete data.
	  Manifest sh (U.Array e)

          -- | An array represented as a function that computes each element.
	| Delayed  sh (sh -> e)


-- Basic Operations -------------------------------------------------------------------------------

-- | Take the shape of an `Array`.
shape	:: Array sh e -> sh
{-# INLINE shape #-}
shape arr
 = case arr of
	Manifest sh _	-> sh
	Delayed  sh _	-> sh


-- | Force an array, so that it becomes `Manifest`.
force	:: (Shape sh, Elt e)
	=> Array sh e -> Array sh e
	
{-# INLINE force #-}
force arr@Manifest{}	
	= arr

force (Delayed sh fn)
 	= Manifest sh
	$ U.map (fn . S.fromIndex sh)
	$ U.enumFromTo 
		(0 :: Int)
		(S.size sh - 1)


-- | Get an indexed element from an array.
--	WARNING: There's no bounds checking with this. 
--               Indexing outside the array will yield badness.
(!:) 	:: (Shape sh, Elt e)
	=> Array sh e -> sh -> e

{-# INLINE (!:) #-}
(!:) arr ix
 = case arr of
	Delayed  _  fn	-> fn ix
	Manifest sh uarr	-> uarr U.!: (S.toIndex sh ix)


-- | Convert a zero dimensional array into a scalar value.
toScalar :: Elt e => Array Z e -> e
{-# INLINE toScalar #-}
toScalar arr
 = case arr of
	Delayed  _ fn		-> fn Z
	Manifest _ uarr		-> uarr U.!: 0


-- Conversion -------------------------------------------------------------------------------------
-- | Convert a list to an `Array`.
--	The length of the list must be exactly the size of the shape, 
--	else an exception will be thrown.
fromList 
	:: forall sh e
	.  (Shape sh, Elt e)
	=> sh				-- ^ Shape of resulting array.
	-> [e]				-- ^ List to convert.
	-> Array sh e
	
fromList sh xx
	| U.length uarr /= S.size sh
	= error $ unlines
	 	[ stage ++ ".fromList: size of array shape does not match size of list"
		, "        size of shape = " ++ (show $ S.size sh) 	++ "\n"
		, "        size of list  = " ++ (show $ U.length uarr) 	++ "\n" ]
	
	| otherwise
	= Manifest sh uarr

	where	uarr	= U.fromList xx
	
	
-- | Convert an `Array` to a list.
toList 	:: (Shape sh, Elt e)
	=> Array sh e
	-> [e]

toList arr
 = case force arr of
	Manifest _ uarr	-> U.toList uarr
	_		-> error $ stage ++ ".toList: force failed"


-- Instances --------------------------------------------------------------------------------------

-- Eq
instance (Elt e, Eq e, Shape sh) => Eq (Array sh e) where
	(==) arr1  arr2 
		= toScalar 
		$ fold (&&) True 
		$ (flip reshape) (Z :. (S.size $ shape arr1)) 
		$ zipWith (==) arr1 arr2
		
	(/=) a1 a2 
		= not $ (==) a1 a2


-- Reshaping --------------------------------------------------------------------------------------
reshape	:: (Shape sh, Shape sh', Elt e) 
	=> Array sh e 			-- ^ Source Array.
	-> sh'				-- ^ New Shape.
	-> Array sh' e

{-# INLINE reshape #-}
reshape arr newShape
	| not $ S.size newShape == S.size (shape arr)
	= error $ stage ++ ".reshape: reshaped array will not match size of the original"
	
	| otherwise
	= Delayed newShape $ ((arr !:) . (S.fromIndex (shape arr)) . (S.toIndex newShape))


-- Traversal --------------------------------------------------------------------------------------



-- Computations ----------------------------------------------------------------------------------

-- | Fold the innermost dimension. 
--	Combine this with `transpose` to fold any other dimension.
fold 	:: (Elt e, Shape dim) 
	=> (e -> e -> e) 
	-> e 
	-> Array (dim :. Int)  e 
	-> Array dim e

{-# INLINE fold #-}
fold f x arr
 = let	sh' :. n	= shape arr
	elemFn i 	= U.fold f x
			$ U.map (\ix -> arr !: (i :. ix)) 
				(U.enumFromTo 0 (n - 1))

   in	Delayed sh' elemFn


-- | Combine two arrays, element-wise, with a binary operator.
--	If the size of two array arguments differ in a dimension, the resulting
--   	array's shape is their intersection.
zipWith :: (Elt a, Elt b, Elt c, Shape sh) 
	=> (a -> b -> c) 
	-> Array sh a
	-> Array sh b
	-> Array sh c

{-# INLINE zipWith #-}
zipWith f arr1 arr2
 = let	sh'	= S.intersectDim (shape arr1) (shape arr2)
	fn i	= f (arr1 !: i) (arr2 !: i)
   in	Delayed sh' fn


-- | Sum all the elements in the array.
sumAll	:: (Elt e, Shape dim, Num e)
	=> Array dim e
	-> e

sumAll arr
	= U.fold (+) 0
	$ U.map ((arr !:) . (S.fromIndex (shape arr)))
	$ U.enumFromTo
		0
		((S.size $ shape arr) - 1)


-- Tests ------------------------------------------------------------------------------------------

-- | QuickCheck Properties.
tests_DataArrayRepaArray :: [(String, Property)]
tests_DataArrayRepaArray
 = 	[ ("forceIsId/DIM5",		property prop_forceIsId_DIM5)
	, ("toListFromList/DIM3",	property prop_toListFromList_DIM3) 
	, ("sumIsSum/DIM3",		property prop_sumIsSum_DIM3) ]


-- The Eq instance uses fold and zipWith.
prop_forceIsId_DIM5
 = 	forAll (arbitrarySmallShape 10)			$ \(sh :: DIM5)  ->
	forAll (arbitraryListOfLength  (S.size sh))	$ \(xx :: [Int]) ->
	let arr	= fromList sh xx
	in  arr == force arr


prop_toListFromList_DIM3
 = 	forAll (arbitrarySmallShape 10)			$ \(sh :: DIM3) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) ->
	toList (fromList sh xx) == xx


prop_sumIsSum_DIM3
 = 	forAll (arbitrarySmallShape 100)		$ \(sh :: DIM2) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) -> 
	sumAll (fromList sh xx) == P.sum xx




