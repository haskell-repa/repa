{-# LANGUAGE BangPatterns, TypeOperators, FlexibleContexts #-}

module CArray
	( CArray(..)

	-- * Conversions
        , genCArray
        , mkCArray
        , carrayData
	, toCArray
	, fromCArray
	, toList

	, forceCArray 
	, toScalar
	, (!:)
	, traverseCArray
        , traverse2CArray
	, reshape
	, transpose
	, backpermute
        , append
	, appendWithShape
	, replicateSlice
	
	-- * Computations
	, map
	, zipWith
	, fold
	, sum
	, sumAll)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted.Gabi	(mapU, foldU, enumFromToU)

import qualified Array 				as A
import Array					((:.)(..))
import Prelude 					hiding (map, zip, zipWith, replicate, sum)
import Data.Maybe
import Data.Either


-- CArray -----------------------------------------------------------------------------------------
data CArray dim e 
	= CArray
	{ carrayShape	:: dim
	, carrayCache	:: Either (dim -> e) (U.Array e) }

infixr 0 `deepSeqCArray`
deepSeqCArray :: A.Shape dim => CArray dim e -> a -> a
{-# INLINE deepSeqCArray #-}
deepSeqCArray (CArray sh r) x = sh `A.deepSeq`
                                case r of
                                  Left _    -> x
                                  Right arr -> arr `seq` x

genCArray :: A.Shape dim => dim -> (dim -> a) -> CArray dim a
{-# INLINE genCArray #-}
genCArray sh f = sh `A.deepSeq` CArray sh (Left f)

mkCArray :: A.Shape dim => dim -> U.Array a -> CArray dim a
{-# INLINE mkCArray #-}
mkCArray sh arr = CArray sh (Right arr)

carrayData :: (A.Shape dim, U.Elt a) => CArray dim a -> U.Array a
{-# INLINE carrayData #-}
carrayData arr = case forceCArray arr of
                   CArray _ (Right xs) -> xs
	

-- Instances --------------------------------------------------------------------------------------

-- Show
instance (U.Elt e, A.Shape dim, Show e) => Show (CArray dim e) where
 	show arr = show $ fromCArray arr

-- Eq
instance (U.Elt e, Eq e, A.Shape sh) => Eq (CArray sh e) where
	(==) arr1@(CArray sh _)  arr2 
		= toScalar 
		$ fold (&&) True 
		$ (flip reshape) (() :. (A.size sh)) 
		$ zipWith (==) arr1 arr2
		
	(/=) a1 a2 
		= not $ (==) a1 a2

-- Num
-- All operators apply elementwise.
instance (U.Elt e, A.Shape dim, Num e) => Num (CArray dim e) where
	(+)		= zipWith (+)
	(-)		= zipWith (-)
	(*)		= zipWith (*)
	negate  	= map negate
	abs		= map abs
	signum 		= map signum

	fromInteger n	= CArray fail $ Left $ (\_ -> fromInteger n) 
	 where fail	= error "CArray.fromInteger: Constructed array has no shape"


-- Conversions ------------------------------------------------------------------------------------
-- | Convert a strict array into a cached array.
toCArray :: (U.Elt e, A.Shape dim) => A.Array dim e -> CArray dim e
{-# INLINE toCArray #-}
toCArray (A.Array sh dat)
  = sh `A.deepSeq` dat `seq`
    CArray { carrayShape = sh
	   , carrayCache = Right dat }
		

-- | Convert a cache array into a strict array
fromCArray :: (U.Elt e, A.Shape dim) => CArray dim e -> A.Array dim e
{-# INLINE fromCArray #-}
fromCArray (CArray shape cache)
 = shape `A.deepSeq`
   A.Array
	{ A.arrayData
		= case cache of
			Left fn
			 -> U.map (fn . A.fromIndex shape)
			  $ U.enumFromTo 
				(0 :: Int)
				((A.size shape) - 1)
				
			Right uarr -> uarr
			
	, A.arrayShape = shape }


-- | Convert a `CArray` to a flat list of its elements.
toList	:: (U.Elt e, A.Shape dim)
	=> CArray dim e 
	-> [e]

toList arr
	= U.toList
	$ U.map ((arr !:) . (A.fromIndex $ carrayShape arr))
	$ U.enumFromTo 
		(0 :: Int)
		((A.size $ carrayShape arr) - 1)
	
	
-- Forcing ----------------------------------------------------------------------------------------
forceCArray 
	:: (U.Elt e, A.Shape dim) 
	=> CArray dim e
	-> CArray dim e

{-# INLINE forceCArray #-}
forceCArray arr = toCArray (fromCArray arr)


-- Constructors -----------------------------------------------------------------------------------
-- | Convert a zero dimensional array into a scalar value.
toScalar :: U.Elt e => CArray () e -> e
toScalar (CArray _ m) 
	= case m of
		Left fn 	-> fn ()
		Right uarr	-> uarr U.!: 0
		
			

-- Primitive functions ----------------------------------------------------------------------------
-- | Lookup the value in an array.
(!:) 	:: (A.Shape dim, U.Elt e)
	=> CArray dim e -> dim -> e

{-# INLINE (!:) #-}
(!:) arr ix
 = case carrayCache arr of
	Right uarr	-> uarr U.!: (A.toIndex (carrayShape arr) ix)
	Left  fn	-> fn ix


-- Traversing -------------------------------------------------------------------------------------

-- | Transform and traverse all the elements of an array.
traverseCArray
	:: (U.Elt a, A.Shape dim)
	=> CArray dim a			-- ^ Source array.
	-> (dim  -> dim')		-- ^ Fn to transform the shape of the array.
	-> ((dim -> a) -> dim' -> b)	-- ^ Fn to produce elements of the result array, 
					--	it is passed a fn to get elements of the source array.
	-> CArray dim' b
	
{-# INLINE traverseCArray #-}
traverseCArray arr@(CArray sh _) dFn trafoFn
	= CArray (dFn sh) (Left $ trafoFn (arr !:))


traverse2CArray :: (A.Shape dim, A.Shape dim', A.Shape dim'',
                    U.Elt e, U.Elt f, U.Elt g)
                => CArray dim e -> CArray dim' f
                -> (dim -> dim' -> dim'')
                -> ((dim -> e) -> (dim' -> f) -> (dim'' -> g))
                -> CArray dim'' g 
{-# INLINE traverse2CArray #-}
traverse2CArray xs@(CArray sh1 _) ys@(CArray sh2 _) f g
  = xs `deepSeqCArray` ys `deepSeqCArray`
    CArray (f sh1 sh2) (Left $ g ((!:) xs) ((!:) ys))


-- Reshaping -------------------------------------------------------------------------------------
reshape	:: (A.Shape dim, A.Shape dim', U.Elt e) 
	=> CArray dim e 		-- ^ Source Array.
	-> dim'				-- ^ New Shape.
	-> CArray dim' e

{-# INLINE reshape #-}
reshape arr@(CArray shape fn) newShape
	| not $ A.size newShape == A.size shape
	= error "CArray.reshape: reshaped array will not match size of the original"
	
	| otherwise
	= CArray newShape $ Left $ ((arr !:) . (A.fromIndex shape) . (A.toIndex newShape))

	
-- Transposing -----------------------------------------------------------------------------------
-- | Transpose the lowest two dimensions of a matrix.
transpose 
	:: (A.Shape dim, U.Elt a) 
	=> CArray (dim :. Int :. Int) a
	-> CArray (dim :. Int :. Int) a

{-# INLINE transpose #-}
transpose arr 
 = traverseCArray arr
	(\(sh :. m :. n) 	-> (sh :. n :.m))
	(\f -> \(sh :. i :. j) 	-> f (sh :. j :. i))


-- Permutation ------------------------------------------------------------------------------------
-- | Generalised array backpermutation.
backpermute
	:: (U.Elt e, A.Shape dim, A.Shape dim') 
	=> CArray dim e 		-- ^ Source array.
	-> dim' 			-- ^ Target shape.
	-> (dim' -> dim) 		-- ^ Fn mapping each index in the target shape range
					--	to an index of the source array range.
	-> CArray dim' e

{-# INLINE backpermute #-}
backpermute arr@(CArray shape _) newSh fn' 
	= CArray newSh $ Left ((arr !:) . fn')



-- Append -----------------------------------------------------------------------------------------

-- | Append two arrays
append	:: (U.Elt e, A.Shape sh)
	=> CArray (sh :. Int) e
	-> CArray (sh :. Int) e
	-> CArray (sh :. Int) e

{-# INLINE append #-}
append arr1 arr2 
  = traverse2CArray arr1 arr2 shFn f
  where
   	(_ :. n) 	= carrayShape arr1
    	(_ :. m) 	= carrayShape arr2

	shFn (sh :. n) (_  :. m) 
			= sh :. (n + m)

	f f1 f2 (sh :. i)
          | i < n	= f1 (sh :. i)
	  | otherwise	= f2 (sh :. (i - n))


appendWithShape
 	:: (U.Elt e, A.Shape sh)
       	=> CArray (sh :. Int) e
	-> CArray (sh :. Int) e
	-> (sh :. Int)			-- ^ shape of resulting array
	-> CArray (sh :. Int) e

{-# INLINE appendWithShape #-}
appendWithShape arr1@(CArray sh1 _) arr2@(CArray sh2 _) newSh
  = sh1 `A.deepSeq` sh2 `A.deepSeq` CArray newSh (Left elemFn)
  where
	(_ :. n)	= sh1
	(_ :. m)	= sh2

	elemFn ix@(sh :. i) 
	 = if A.inRange A.zeroDim sh1 ix
		then arr1 !: ix
		else arr2 !: (sh :. (i - n))


-- Replication ------------------------------------------------------------------------------------
replicateSlice
	:: ( U.Elt e, A.Slice sl
	   , A.Shape (A.FullShape sl)
	   , A.Shape (A.SliceShape sl)) 
	=> CArray (A.SliceShape sl) e 	-- ^ Source array.
	-> sl  				-- ^ Slice to replicate.
	-> CArray (A.FullShape sl) e

{-# INLINE replicateSlice #-}
replicateSlice arr@(CArray shape _ ) sl
	= backpermute arr (A.repShape sl shape) (A.repInd sl)


-- Computations -----------------------------------------------------------------------------------
-- | Map a worker function over each element of n-dim CArray.
map	:: (U.Elt a, U.Elt b, A.Shape dim) 
	=> (a -> b) 			-- ^ Worker function.
	-> CArray dim a			-- ^ Source array.
	-> CArray dim b

{-# INLINE map #-}
map f arr@(CArray shape _) 
	= CArray shape $ Left $ (f . (arr !:))


-- | If the size of two array arguments differ in a dimension, the resulting
--   array's shape is the minimum of the two 
zipWith :: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) 
	=> (a -> b -> c) 
	-> CArray dim a
	-> CArray dim b
	-> CArray dim c

{-# INLINE zipWith #-}
zipWith f arr1 arr2
	= CArray (A.intersectDim 
			(carrayShape arr1)
			(carrayShape arr2))
		 (Left (\i -> f (arr1 !: i) (arr2 !: i)))


-- | Fold the innermost dimension. 
--	Combine with `transpose` to fold any other dimension.
fold 	:: (U.Elt e, A.Shape dim) 
	=> (e -> e -> e) 
	-> e 
	-> CArray (dim :. Int)  e 
	-> CArray dim e

{-# INLINE fold #-}
fold f n arr@(CArray sh@(sh' :. s) _) 
	= CArray sh' $ Left elemFn
	where	elemFn i = foldU f n (mapU (\s -> arr !: (i :. s)) (enumFromToU 0 (s - 1)))


-- | Sum the innermost dimension.
sum	:: (U.Elt e, A.Shape dim, Num e)
	=> CArray (dim :. Int) e
	-> CArray dim e

sum arr	= fold (+) 0 arr


-- | Sum all the elements in the array.
sumAll	:: (U.Elt e, A.Shape dim, Num e)
	=> CArray dim e
	-> e

sumAll arr
	= U.fold (+) 0
	$ U.map ((arr !:) . (A.fromIndex (carrayShape arr)))
	$ U.enumFromTo
		0
		((A.size $ carrayShape arr) - 1)

