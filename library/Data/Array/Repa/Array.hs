{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

-- | High-performance, regular, shape-polymorphic parallel arrays. 
--
--   WARNING: 	Most of the functions that operate on indices don't perform bounds checks.
--		Performing these checks would interfere with code optimisation and reduce performance.		
--		Indexing outside arrays, or failing to meet the stated obligations will
--		likely cause heap corruption.
module Data.Array.Repa.Array
	( Array	(..)

	 -- * Basic Operations
	, shape
	, force
	, (!:)
	, unit
	, toScalar
	, deepSeqArray
	
	 -- * Conversion
	, fromList
	, toList

	 -- * Index space transformations
	, reshape
	, append
	, backpermute
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
	, traverse2
		
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
data Array sh a
	= -- | An array represented as some concrete data.
	  Manifest sh (U.Array a)

          -- | An array represented as a function that computes each element.
	| Delayed  sh (sh -> a)


-- Basic Operations -------------------------------------------------------------------------------
-- | Take the shape of an array.
shape	:: Array sh a -> sh

{-# INLINE shape #-}
shape arr
 = case arr of
	Manifest sh _	-> sh
	Delayed  sh _	-> sh


-- | Force an array, so that it becomes `Manifest`.
force	:: (Shape sh, Elt a)
	=> Array sh a -> Array sh a
	
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
--
--   OBLIGATION: The index must be within the array. 
--
-- 	@inRange zeroDim (shape arr) ix == True@
--
(!:) 	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array sh a 		-- ^ Source array (@arr@).
	-> sh 			-- ^ Index (@ix@).
	-> a

{-# INLINE (!:) #-}
(!:) arr ix
 = case arr of
	Delayed  _  fn	-> fn ix
	Manifest sh uarr	-> uarr U.!: (S.toIndex sh ix)


-- | Wrap a scalar into a singleton array.
unit :: Elt a => a -> Array Z a
{-# INLINE unit #-}
unit 	= Delayed Z . const


-- | Take the scalar value from a singleton array.
toScalar :: Elt a => Array Z a -> a
{-# INLINE toScalar #-}
toScalar arr
 = case arr of
	Delayed  _ fn		-> fn Z
	Manifest _ uarr		-> uarr U.!: 0


-- | Ensure an array's structure is fully evaluated.
--	This evaluates the shape and outer constructor, but does not `force` the elements.
infixr 0 `deepSeqArray`
deepSeqArray 
	:: Shape sh
	=> Array sh a 
	-> b -> b

{-# INLINE deepSeqArray #-}
deepSeqArray arr x 
 = case arr of
	Delayed  sh _		-> sh `S.deepSeq` x
	Manifest sh uarr	-> sh `S.deepSeq` uarr `seq` x


-- Conversion -------------------------------------------------------------------------------------
-- | Convert a list to an array.
-- 
--	The length of the list must be exactly the `size` of the shape, else `error`.
--
fromList 
	:: forall sh a
	.  (Shape sh, Elt a)
	=> sh				-- ^ Shape of result array.
	-> [a]				-- ^ List to convert.
	-> Array sh a
	
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
	
	
-- | Convert an array to a list.
toList 	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array sh a
	-> [a]

{-# INLINE toList #-}
toList arr
 = case force arr of
	Manifest _ uarr	-> U.toList uarr
	_		-> error $ stage ++ ".toList: force failed"


-- Instances --------------------------------------------------------------------------------------

-- Show
instance (Shape sh, Elt a, Show a) => Show (Array sh a) where
 	show arr = show $ toList arr

-- Eq
instance (Shape sh, Elt a, Eq a) => Eq (Array sh a) where

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
instance (Shape sh, Elt a, Num a) => Num (Array sh a) where
	(+)		= zipWith (+)
	(-)		= zipWith (-)
	(*)		= zipWith (*)
	negate  	= map negate
	abs		= map abs
	signum 		= map signum

	fromInteger n	 = Delayed failShape (\_ -> fromInteger n) 
	 where failShape = error $ stage ++ ".fromInteger: Constructed array has no shape."


-- Index space transformations --------------------------------------------------------------------
-- | Impose a new shape on the same elements.
--
--	The new shape must be the same size as the original, else `error`.
--
reshape	:: forall sh sh' a
	.  (Shape sh, Shape sh', Elt a) 
	=> Array sh a 			-- ^ Source array.
	-> sh'				-- ^ Shape of result array.
	-> Array sh' a

{-# INLINE reshape #-}
reshape arr newShape
	| not $ S.size newShape == S.size (shape arr)
	= error $ stage ++ ".reshape: reshaped array will not match size of the original"
	
	| otherwise
	= Delayed newShape 
	$ ((arr !:) . (S.fromIndex (shape arr)) . (S.toIndex newShape))


-- | Append two arrays.
--
--   OBLIGATION: The higher dimensions of both arrays must have the same shape and size.
--
--   @tail (listOfShape (shape arr1)) == tail (listOfShape (shape arr2))@
--
append	
	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array (sh :. Int) a		-- ^ First array (@arr1@)
	-> Array (sh :. Int) a		-- ^ Second array (@arr2@)
	-> Array (sh :. Int) a

{-# INLINE append #-}
append arr1 arr2 
 = traverse2 arr1 arr2 fnShape fnElem
 where
 	(_ :. n) 	= shape arr1

	fnShape (sh :. i) (_  :. j) 
		= sh :. (i + j)

	fnElem f1 f2 (sh :. i)
      		| i < n		= f1 (sh :. i)
  		| otherwise	= f2 (sh :. (i - n))


-- | Backwards permutation. 
--	The result array has the same shape as the original.
backpermute
	:: forall sh sh' a
	.  (Shape sh, Shape sh', Elt a) 
	=> Array sh a 			-- ^ Source array.
	-> sh' 				-- ^ Shape of result array.
	-> (sh' -> sh) 			-- ^ Function mapping each index in the target array
					--	to an index of the source array.
	-> Array sh' a

{-# INLINE backpermute #-}
backpermute arr newShape fnIndex
	= Delayed newShape ((arr !:) . fnIndex)


-- | Transpose the lowest two dimensions of an array. 
--	Transposing an array twice yields the original.
transpose 
	:: forall sh a
	.  (Shape sh, Elt a) 
	=> Array (sh :. Int :. Int) a	-- ^ Source array.
	-> Array (sh :. Int :. Int) a

{-# INLINE transpose #-}
transpose arr 
 = traverse arr
	(\(sh :. m :. n) 	-> (sh :. n :.m))
	(\f -> \(sh :. i :. j) 	-> f (sh :. j :. i))


-- Structure Preserving Operations ----------------------------------------------------------------
-- | Apply a worker function to each element of an array, yielding a new array with the same shape.
map	:: forall sh a b
	.  (Shape sh, Elt a, Elt b) 
	=> (a -> b) 			-- ^ Function to transform each element.
	-> Array sh a			-- ^ Source array.
	-> Array sh b

{-# INLINE map #-}
map f arr
	= Delayed (shape arr) (f . (arr !:))


-- | Combine two arrays, element-wise, with a binary operator.
--	If the size of the two array arguments differ in shape, then the resulting
--   	array's shape is their intersection.
zipWith :: (Shape sh, Elt a, Elt b, Elt c) 
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
fold 	:: (Shape sh, Elt a)
	=> (a -> a -> a)
	-> a 
	-> Array (sh :. Int) a
	-> Array sh a

{-# INLINE fold #-}
fold f x arr
 = let	sh' :. n	= shape arr
	elemFn i 	= U.fold f x
			$ U.map (\ix -> arr !: (i :. ix)) 
				(U.enumFromTo 0 (n - 1))

   in	Delayed sh' elemFn


-- | Sum the innermost dimension.
sum	:: (Shape sh, Elt a, Num a)
	=> Array (sh :. Int) a
	-> Array sh a

{-# INLINE sum #-}
sum arr	= fold (+) 0 arr


-- | Sum all the elements.
sumAll	:: (Shape sh, Elt a, Num a)
	=> Array sh a
	-> a

{-# INLINE sumAll #-}
sumAll arr
	= U.fold (+) 0
	$ U.map ((arr !:) . (S.fromIndex (shape arr)))
	$ U.enumFromTo
		0
		((S.size $ shape arr) - 1)


-- Generic Traversal -----------------------------------------------------------------------------
-- | Unstructured traversal.
--
traverse
	:: forall sh sh' a b
	.  (Shape sh, Shape sh', Elt a)
	=> Array sh a				-- ^ Source array.
	-> (sh  -> sh')				-- ^ Function to produce the shape of the result.
	-> ((sh -> a) -> sh' -> b)		-- ^ Function to produce elements of the result. 
	 					--   It is passed a lookup function to get elements of the source.
	-> Array sh' b
	
{-# INLINE traverse #-}
traverse arr transformShape newElem
	= Delayed 
		(transformShape (shape arr)) 
		(newElem        (arr !:))


-- | Unstructured traversal over two arrays at once.
traverse2
	:: forall sh sh' sh'' a b c
	.  ( Shape sh, Shape sh', Shape sh''
	   , Elt a,    Elt b,     Elt c)
        => Array sh a 				-- ^ First source array.
	-> Array sh' b				-- ^ Second source array.
        -> (sh -> sh' -> sh'')			-- ^ Function to produce the shape of the result.
        -> ((sh -> a) -> (sh' -> b) 		
                      -> (sh'' -> c))		-- ^ Function to produce elements of the result.
						--   It is passed lookup functions to get elements of the 
						--   source arrays.
        -> Array sh'' c 

{-# INLINE traverse2 #-}
traverse2 arrA arrB fnShape fnElem
	= arrA `deepSeqArray` arrB `deepSeqArray`
   	  Delayed 
		(fnShape (shape arrA) (shape arrB)) 
		(fnElem ((!:) arrA) ((!:) arrB))



-- Arbitrary --------------------------------------------------------------------------------------
-- | Create an arbitrary small array, restricting the size of each of the dimensions.
arbitrarySmallArray 
	:: forall sh a
	.  (Shape sh, Elt a, Arbitrary a)
	=> Int 				-- ^ Maximum size of each dimension.
	-> Gen (Array (sh :. Int) a)

arbitrarySmallArray maxDim
 = do	sh	<- arbitrarySmallShape maxDim
	xx	<- arbitraryListOfLength (S.size sh)
	return	$ fromList sh xx



-- Tests ------------------------------------------------------------------------------------------

-- | QuickCheck Properties.
tests_DataArrayRepaArray :: [(String, Property)]
tests_DataArrayRepaArray
 = [(stage ++ "." ++ name, test) | (name, test)
    <-	[ ("id_force/DIM5",			property prop_id_force_DIM5)
	, ("id_toScalarUnit",			property prop_id_toScalarUnit)
	, ("id_toListFromList/DIM3",		property prop_id_toListFromList_DIM3) 
	, ("id_transpose/DIM4",			property prop_id_transpose_DIM4)
	, ("reshapeTransposeSize/DIM3",		property prop_reshapeTranspose_DIM3)
	, ("appendIsAppend/DIM3",		property prop_appendIsAppend_DIM3)
	, ("sumAllIsSum/DIM3",			property prop_sumAllIsSum_DIM3) ]]


-- The Eq instance uses fold and zipWith.
prop_id_force_DIM5
 = 	forAll (arbitrarySmallArray 10)			$ \(arr :: Array DIM5 Int) ->
	arr == force arr
	
prop_id_toScalarUnit (x :: Int)
 =	toScalar (unit x) == x

-- Conversions ------------------------
prop_id_toListFromList_DIM3
 =	forAll (arbitrarySmallShape 10)			$ \(sh :: DIM3) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) ->
	toList (fromList sh xx) == xx

-- Index Space Transforms -------------
prop_id_transpose_DIM4
 = 	forAll (arbitrarySmallArray 20)			$ \(arr :: Array DIM3 Int) ->
	transpose (transpose arr) == arr

-- A reshaped array has the same size and sum as the original
prop_reshapeTranspose_DIM3
 = 	forAll (arbitrarySmallArray 20)			$ \(arr :: Array DIM3 Int) ->
   let	arr'	= transpose arr
   	sh'	= shape arr'
   in	(S.size $ shape arr) == S.size (shape (reshape arr sh'))
     && (sumAll arr          == sumAll arr')

prop_appendIsAppend_DIM3
 = 	forAll (arbitrarySmallArray 20)			$ \(arr1 :: Array DIM3 Int) ->
	sumAll (append arr1 arr1) == (2 * sumAll arr1)

-- Reductions --------------------------
prop_sumAllIsSum_DIM3
 = 	forAll (arbitrarySmallShape 100)		$ \(sh :: DIM2) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) -> 
	sumAll (fromList sh xx) == P.sum xx


