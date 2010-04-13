{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

-- | High-performance, regular, shape-polymorphic parallel arrays. 
--
--   WARNING: 	Most of the functions that operate on indices don't perform bounds checks.
--		Doing these checks would interfere with code optimisation and reduce performance.		
--		Indexing outside arrays, or failing to meet the stated obligations will
--		likely cause heap corruption.
module Data.Array.Repa
	( module Data.Array.Repa.Shape
	, module Data.Array.Repa.Index
	, module Data.Array.Repa.Slice
	
	, Array	(..)

	 -- * Constructors
	, fromUArray
	, fromFunction
	, unit

	 -- * Projections
	, extent
	, delay
	, toUArray
	, index, (!:)
	, toScalar

	 -- * Basic Operations
	, force
	, deepSeqArray
	
	 -- * Conversion
	, fromList
	, toList

	 -- * Index space transformations
	, reshape
	, append, (+:+)
	, transpose
	, replicate
	, backpermute
	, backpermuteDft

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
	, props_DataArrayRepaArray)
where
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Shape
import Data.Array.Repa.QuickCheck
import Test.QuickCheck
import Data.Array.Parallel.Unlifted		(Elt)
import qualified Data.Array.Repa.Shape		as S
import qualified Data.Array.Parallel.Unlifted	as U
import Prelude					hiding (sum, map, zipWith, replicate)	
import qualified Prelude			as P

stage	= "Data.Array.Repa.Array"
	
	
-- | Possibly delayed arrays.
data Array sh a
	= -- | An array represented as some concrete unboxed data.
	  Manifest sh (U.Array a)

          -- | An array represented as a function that computes each element.
	| Delayed  sh (sh -> a)

-- Constructors ----------------------------------------------------------------------------------

-- | Create a `Manifest` array from an unboxed `U.Array`. 
--	The elements are in row-major order.
fromUArray
	:: Shape sh
	=> sh
	-> U.Array a
	-> Array sh a

{-# INLINE fromUArray #-}
fromUArray sh uarr
	= sh   `S.deepSeq` 
	  uarr `seq`
	  Manifest sh uarr


-- | Create a `Delayed` array from a function.
fromFunction 
	:: Shape sh
	=> sh
	-> (sh -> a)
	-> Array sh a
	
{-# INLINE fromFunction #-}
fromFunction sh fnElems
	= sh `S.deepSeq` Delayed sh fnElems


-- | Wrap a scalar into a singleton array.
unit :: Elt a => a -> Array Z a
{-# INLINE unit #-}
unit 	= Delayed Z . const


-- Projections ------------------------------------------------------------------------------------

-- | Take the extent of an array.
extent	:: Array sh a -> sh
{-# INLINE extent #-}
extent arr
 = case arr of
	Manifest sh _	-> sh
	Delayed  sh _	-> sh

-- | Unpack an array into delayed form.
delay 	:: (Shape sh, Elt a) 
	=> Array sh a 
	-> (sh, sh -> a)

{-# INLINE delay #-}	
delay arr
 = case arr of
	Manifest sh uarr	-> (sh, \i -> uarr U.!: S.toIndex sh i)
	Delayed  sh fn		-> (sh, fn)


-- | Convert an array to an unboxed `U.Array`, forcing it if required.
--	The elements come out in row-major order.
toUArray 
	:: (Shape sh, Elt a)
	=> Array sh a 
	-> U.Array a
{-# INLINE toUArray #-}
toUArray arr
 = case force arr of
	Manifest _ uarr	-> uarr
	_		-> error $ stage ++ ".toList: force failed"


-- | Get an indexed element from an array.
--
--   OBLIGATION: The index must be within the array. 
--
-- 	@inRange zeroDim (shape arr) ix == True@
--
index, (!:)
	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array sh a
	-> sh 
	-> a

{-# INLINE index #-}
index arr ix
 = case arr of
	Delayed  _  fn	-> fn ix
	Manifest sh uarr	-> uarr U.!: (S.toIndex sh ix)

(!:) arr ix = index arr ix


-- | Take the scalar value from a singleton array.
toScalar :: Elt a => Array Z a -> a
{-# INLINE toScalar #-}
toScalar arr
 = case arr of
	Delayed  _ fn		-> fn Z
	Manifest _ uarr		-> uarr U.!: 0


-- Basic Operations -------------------------------------------------------------------------------

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
	
	
-- | Ensure an array's structure is fully evaluated.
--	This evaluates the extent and outer constructor, but does not `force` the elements.
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
--	The length of the list must be exactly the `size` of the extent given, else `error`.
--
fromList
	:: (Shape sh, Elt a)
	=> sh
	-> [a]
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
toList 	:: (Shape sh, Elt a)
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
		$ (flip reshape) (Z :. (S.size $ extent arr1)) 
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
-- | Impose a new shape on the elements of an array.
--	The new extent must be the same size as the original, else `error`.
--
reshape	:: (Shape sh, Shape sh', Elt a) 
	=> Array sh a
	-> sh'
	-> Array sh' a

{-# INLINE reshape #-}
reshape arr newExtent
	| not $ S.size newExtent == S.size (extent arr)
	= error $ stage ++ ".reshape: reshaped array will not match size of the original"
	
	| otherwise
	= Delayed newExtent
	$ ((arr !:) . (S.fromIndex (extent arr)) . (S.toIndex newExtent))


-- | Append two arrays.
--
--   OBLIGATION: The higher dimensions of both arrays must have the same extent.
--
--   @tail (listOfShape (shape arr1)) == tail (listOfShape (shape arr2))@
--
append, (+:+)	
	:: (Shape sh, Elt a)
	=> Array (sh :. Int) a
	-> Array (sh :. Int) a
	-> Array (sh :. Int) a

{-# INLINE append #-}
append arr1 arr2 
 = traverse2 arr1 arr2 fnExtent fnElem
 where
 	(_ :. n) 	= extent arr1

	fnExtent (sh :. i) (_  :. j) 
		= sh :. (i + j)

	fnElem f1 f2 (sh :. i)
      		| i < n		= f1 (sh :. i)
  		| otherwise	= f2 (sh :. (i - n))

{-# INLINE (+:+) #-}
(+:+) arr1 arr2 = append arr1 arr2


-- | Transpose the lowest two dimensions of an array. 
--	Transposing an array twice yields the original.
transpose 
	:: (Shape sh, Elt a) 
	=> Array (sh :. Int :. Int) a
	-> Array (sh :. Int :. Int) a

{-# INLINE transpose #-}
transpose arr 
 = traverse arr
	(\(sh :. m :. n) 	-> (sh :. n :.m))
	(\f -> \(sh :. i :. j) 	-> f (sh :. j :. i))


-- | Replicate an array according to a given slice specification.
replicate
	:: ( Slice sl
	   , Shape (FullShape sl)
	   , Shape (SliceShape sl)
	   , Elt e)
	=> sl
	-> Array (SliceShape sl) e
	-> Array (FullShape sl) e

{-# INLINE replicate #-}
replicate slice arr
	= backpermute 
		(fullOfSlice slice (extent arr)) 
		(sliceOfFull slice)
		arr
		

-- | Backwards permutation of an array's elements.
--	The result array has the same extent as the original.
backpermute
	:: forall sh sh' a
	.  (Shape sh, Shape sh', Elt a) 
	=> sh' 				-- ^ Extent of result array.
	-> (sh' -> sh) 			-- ^ Function mapping each index in the result array
					--	to an index of the source array.
	-> Array sh a 			-- ^ Source array.
	-> Array sh' a

{-# INLINE backpermute #-}
backpermute newExtent perm arr
	= traverse arr (const newExtent) (. perm) 
	

-- | Default backwards permutation of an array's elements.
--	If the function returns `Nothing` then the value at that index is taken
--	from the default array (@arrDft@)
backpermuteDft
	:: forall sh sh' a
	.  (Shape sh, Shape sh', Elt a) 
	=> Array sh' a			-- ^ Default values (@arrDft@)
	-> (sh' -> Maybe sh) 		-- ^ Function mapping each index in the result array
					--	to an index in the source array.
	-> Array sh  a			-- ^ Source array.
	-> Array sh' a

{-# INLINE backpermuteDft #-}
backpermuteDft arrDft fnIndex arrSrc
	= Delayed (extent arrDft) fnElem
	where	fnElem ix	
		 = case fnIndex ix of
			Just ix'	-> arrSrc !: ix'
			Nothing		-> arrDft !: ix
				

-- Structure Preserving Operations ----------------------------------------------------------------
-- | Apply a worker function to each element of an array, 
--	yielding a new array with the same extent.
map	:: (Shape sh, Elt a, Elt b) 
	=> (a -> b)
	-> Array sh a
	-> Array sh b

{-# INLINE map #-}
map f arr
	= Delayed (extent arr) (f . (arr !:))


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
 = let	sh'	= S.intersectDim (extent arr1) (extent arr2)
	fn i	= f (arr1 !: i) (arr2 !: i)
   in	Delayed sh' fn



-- Reductions -------------------------------------------------------------------------------------
-- | Fold the innermost dimension of an array.
--	Combine this with `transpose` to fold any other dimension.
fold 	:: (Shape sh, Elt a)
	=> (a -> a -> a)
	-> a 
	-> Array (sh :. Int) a
	-> Array sh a

{-# INLINE fold #-}
fold f x arr
 = let	sh' :. n	= extent arr
	elemFn i 	= U.fold f x
			$ U.map (\ix -> arr !: (i :. ix)) 
				(U.enumFromTo 0 (n - 1))

   in	Delayed sh' elemFn


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
	= U.fold (+) 0
	$ U.map ((arr !:) . (S.fromIndex (extent arr)))
	$ U.enumFromTo
		0
		((S.size $ extent arr) - 1)


-- Generic Traversal -----------------------------------------------------------------------------
-- | Unstructured traversal.
traverse
	:: forall sh sh' a b
	.  (Shape sh, Shape sh', Elt a)
	=> Array sh a				-- ^ Source array.
	-> (sh  -> sh')				-- ^ Function to produce the extent of the result.
	-> ((sh -> a) -> sh' -> b)		-- ^ Function to produce elements of the result. 
	 					--   It is passed a lookup function to get elements of the source.
	-> Array sh' b
	
{-# INLINE traverse #-}
traverse arr transExtent newElem
	= Delayed 
		(transExtent (extent arr)) 
		(newElem     (arr !:))


-- | Unstructured traversal over two arrays at once.
traverse2
	:: forall sh sh' sh'' a b c
	.  ( Shape sh, Shape sh', Shape sh''
	   , Elt a,    Elt b,     Elt c)
        => Array sh a 				-- ^ First source array.
	-> Array sh' b				-- ^ Second source array.
        -> (sh -> sh' -> sh'')			-- ^ Function to produce the extent of the result.
        -> ((sh -> a) -> (sh' -> b) 		
                      -> (sh'' -> c))		-- ^ Function to produce elements of the result.
						--   It is passed lookup functions to get elements of the 
						--   source arrays.
        -> Array sh'' c 

{-# INLINE traverse2 #-}
traverse2 arrA arrB transExtent newElem
	= arrA `deepSeqArray` arrB `deepSeqArray`
   	  Delayed 
		(transExtent (extent arrA) (extent arrB)) 
		(newElem     ((!:) arrA) ((!:) arrB))



-- Arbitrary --------------------------------------------------------------------------------------
-- | Create an arbitrary small array, restricting the size of each of the dimensions to some value.
arbitrarySmallArray 
	:: (Shape sh, Elt a, Arbitrary sh, Arbitrary a)
	=> Int
	-> Gen (Array (sh :. Int) a)

arbitrarySmallArray maxDim
 = do	sh	<- arbitrarySmallShape maxDim
	xx	<- arbitraryListOfLength (S.size sh)
	return	$ fromList sh xx



-- Properties -------------------------------------------------------------------------------------

-- | QuickCheck properties for this module and its children.
props_DataArrayRepaArray :: [(String, Property)]
props_DataArrayRepaArray
 =  props_DataArrayRepaIndex
 ++ [(stage ++ "." ++ name, test) | (name, test)
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
   	sh'	= extent arr'
   in	(S.size $ extent arr) == S.size (extent (reshape arr sh'))
     && (sumAll arr          == sumAll arr')

prop_appendIsAppend_DIM3
 = 	forAll (arbitrarySmallArray 20)			$ \(arr1 :: Array DIM3 Int) ->
	sumAll (append arr1 arr1) == (2 * sumAll arr1)

-- Reductions --------------------------
prop_sumAllIsSum_DIM3
 = 	forAll (arbitrarySmallShape 100)		$ \(sh :: DIM2) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) -> 
	sumAll (fromList sh xx) == P.sum xx
