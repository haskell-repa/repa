{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

-- | TODO: the fromInteger instance doesn't make sense.

module Data.Array.Repa.Array
	( Array	(..)

	 -- * Basic Operations
	, force
	, (!:)
	
	 -- * Conversion
	, fromList
	, toList

	 -- * Index space transformations
	, reshape
	, transpose

         -- * Structure preserving operations
	, map
	, zipWith

	 -- * Reductions
	, fold
	, sum
	, sumAll
	
	 -- * Generic traversal
	, traverse
		
	 -- * Testing
	, arbitrarySmallArray
	, tests_DataArrayRepaArray)
where
import Data.Array.Repa.Index
import Data.Array.Repa.QuickCheck
import Test.QuickCheck
import Data.Array.Repa.Shape			(Shape)
import Data.Array.Parallel.Unlifted		(Elt)
import qualified Data.Array.Repa.Shape		as S
import qualified Data.Array.Parallel.Unlifted	as U
import Prelude					hiding (sum, map, zipWith)	
import qualified Prelude			as P

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
	
{-# INLINE fromList #-}
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

{-# INLINE toList #-}
toList arr
 = case force arr of
	Manifest _ uarr	-> U.toList uarr
	_		-> error $ stage ++ ".toList: force failed"


-- Instances --------------------------------------------------------------------------------------

-- Show
instance (Elt e, Shape dim, Show e) => Show (Array dim e) where
 	show arr = show $ toList arr

-- Eq
instance (Elt e, Eq e, Shape sh) => Eq (Array sh e) where

	{-# INLINE (==) #-}
	(==) arr1  arr2 
		= toScalar 
		$ fold (&&) True 
		$ (flip reshape) (Z :. (S.size $ shape arr1)) 
		$ zipWith (==) arr1 arr2
		
	{-# INLINE (/=) #-}
	(/=) a1 a2 
		= not $ (==) a1 a2

-- Num
-- All operators apply elementwise.
instance (Elt e, Shape sh, Num e) => Num (Array sh e) where
	(+)		= zipWith (+)
	(-)		= zipWith (-)
	(*)		= zipWith (*)
	negate  	= map negate
	abs		= map abs
	signum 		= map signum

	fromInteger n	= Delayed fail (\_ -> fromInteger n) 
	 where fail	= error $ stage ++ ".fromInteger: Constructed array has no shape."


-- Index space transformations --------------------------------------------------------------------
-- | Change the shape of an array.
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


-- | Transpose the lowest two dimensions of a matrix.
transpose 
	:: (Shape sh, Elt a) 
	=> Array (sh :. Int :. Int) a
	-> Array (sh :. Int :. Int) a

{-# INLINE transpose #-}
transpose arr 
 = traverse arr
	(\(sh :. m :. n) 	-> (sh :. n :.m))
	(\f -> \(sh :. i :. j) 	-> f (sh :. j :. i))


-- Structure Preserving Operations ----------------------------------------------------------------
-- | Map a worker function over each element of n-dim CArray.
map	:: (Elt a, Elt b, Shape sh) 
	=> (a -> b) 			-- ^ Worker function.
	-> Array sh a			-- ^ Source array.
	-> Array sh b

{-# INLINE map #-}
map f arr
	= Delayed (shape arr) (f . (arr !:))


-- | Combine two arrays, element-wise, with a binary operator.
--	If the size of the two array arguments differ in shape, then the resulting
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



-- Reductions -------------------------------------------------------------------------------------
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


-- Generic Traversal -----------------------------------------------------------------------------
-- | Transform and traverse all the elements of an array.
traverse
	:: (Elt a, Shape sh, Shape sh')
	=> Array sh a			-- ^ Source array.
	-> (sh  -> sh')			-- ^ Fn to transform the shape of the array.
	-> ((sh -> a) -> sh' -> b)	-- ^ Fn to produce elements of the result array, 
					--	it is passed a fn to get elements of the source array.
	-> Array sh' b
	
{-# INLINE traverse #-}
traverse arr fnShape fnElem
	= Delayed (fnShape (shape arr)) (fnElem (arr !:))


-- | Sum the innermost dimension.
sum	:: (Elt e, Shape dim, Num e)
	=> Array (dim :. Int) e
	-> Array dim e

{-# INLINE sum #-}
sum arr	= fold (+) 0 arr


-- | Sum all the elements.
sumAll	:: (Elt e, Shape dim, Num e)
	=> Array dim e
	-> e

{-# INLINE sumAll #-}
sumAll arr
	= U.fold (+) 0
	$ U.map ((arr !:) . (S.fromIndex (shape arr)))
	$ U.enumFromTo
		0
		((S.size $ shape arr) - 1)


-- Arbitrary --------------------------------------------------------------------------------------
-- | Create an arbitrary small array, restricting the size of each of the dimensions.
arbitrarySmallArray 
	:: forall sh e
	.  (Shape sh, Elt e, Arbitrary e)
	=> Int 				-- ^ Maximum size of each dimension.
	-> Gen (Array (sh :. Int) e)

arbitrarySmallArray maxDim
 = do	sh	<- arbitrarySmallShape maxDim
	xx	<- arbitraryListOfLength (S.size sh)
	return	$ fromList sh xx



-- Tests ------------------------------------------------------------------------------------------

-- | QuickCheck Properties.
tests_DataArrayRepaArray :: [(String, Property)]
tests_DataArrayRepaArray
 = 	[ ("id_force/DIM5",			property prop_id_force_DIM5)
	, ("id_toListFromList/DIM3",		property prop_id_toListFromList_DIM3) 
--	, ("id_toScalarFromInteger/DIM3",	property prop_id_toScalarFromInteger_DIM3)
	, ("id_transpose/DIM4",			property prop_id_transpose_DIM4)
	, ("sumAllIsSum/DIM3",			property prop_sumAllIsSum_DIM3) ]


-- The Eq instance uses fold and zipWith.
prop_id_force_DIM5
 = 	forAll (arbitrarySmallArray 10)			$ \(arr :: Array DIM5 Int) ->
	arr == force arr
	

prop_id_toListFromList_DIM3
 =	forAll (arbitrarySmallShape 10)			$ \(sh :: DIM3) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) ->
	toList (fromList sh xx) == xx

{- This doesn't work... 
prop_id_toScalarFromInteger_DIM3 (n :: Integer)
 =	n == (toScalar $ (fromInteger n :: Array DIM3 Int))
-}

prop_id_transpose_DIM4
 = 	forAll (arbitrarySmallArray 20)			$ \(arr :: Array DIM3 Int) ->
	transpose (transpose arr) == arr

prop_sumAllIsSum_DIM3
 = 	forAll (arbitrarySmallShape 100)		$ \(sh :: DIM2) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) -> 
	sumAll (fromList sh xx) == P.sum xx


