{-# LANGUAGE ExplicitForAll, TypeOperators, FlexibleInstances, UndecidableInstances, BangPatterns,
             ExistentialQuantification #-}
module Data.Array.Repa.Internals.Base
	( Array (..)
	, Region(..)
	, Range (..)
	, Rect  (..)
	, Generator(..)
	, deepSeqArray, deepSeqArrays

	, singleton, toScalar
	, extent,    delay
	
	-- * Predicates
	, inRange

	-- * Indexing
	, (!),  index
	, (!?), safeIndex
	, unsafeIndex

	-- * Construction
	, fromFunction	
	, fromVector
	, fromList)
where
import Data.Array.Repa.Index
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Shape			as S
import qualified Data.Vector.Unboxed		as V
import Data.Vector.Unboxed			(Vector)

stage	= "Data.Array.Repa.Array"

-- Array ----------------------------------------------------------------------
-- | Repa arrays. 
data Array sh a
	= Array 
	{ -- | The entire extent of the array.
	  arrayExtent		:: sh

	  -- | Generators for array elements, and which regions they apply in.
	, arrayRegions		:: [Region sh a] }


-- | Holds the generator for a region of the array.
data Region sh a
	= Region 
	{ -- | The range in the array this region applies to.
	  regionRange		:: Range sh

	  -- | Generator for array elements in the region.
	, regionGenerator	:: Generator sh a }


-- | Represents a range of elements in the array.
data Range sh 
	  -- | Entire extent of array.
	= RangeAll

	  -- | A range consisting of a possibly disjoint set of rectangles.
	| RangeRects
	{ rangeMatch	:: sh -> Bool
	, rangeRects	:: [Rect sh] }


-- | A rectangle\/cube of arbitrary dimension.
data Rect sh
	= Rect sh sh


-- | Generates array elements for a given region in an array.
data Generator sh a
	-- | Read elements from a manifest vector.  
	= GenManifest
	{ genVector		:: Vector a }
		
	-- | Apply a function to get the element.
	| GenDelayed
	{ genGetElem		:: sh -> a }
		
	-- | Use a cursor to walk over the elements required.
	| forall cursor
	. GenCursor
	{ genMakeCursor		:: sh -> cursor
	, genShiftCursor	:: sh -> cursor -> cursor
	, genLoadElem		:: cursor -> a }


-- Arrays ---------------------------------------------------------------------
-- | Ensure an array's structure is fully evaluated.
infixr 0 `deepSeqArray`
deepSeqArray :: Shape sh => Array sh a -> b -> b
{-# INLINE deepSeqArray #-}
deepSeqArray (Array ex rgns) x
	= ex `S.deepSeq` rgns `deepSeqRegions` x

-- | Like `deepSeqArray` but seqs all the arrays in a list.
infixr 0 `deepSeqArrays`
deepSeqArrays :: Shape sh => [Array sh a] -> b -> b
{-# INLINE deepSeqArrays #-}
deepSeqArrays arr y
 = case arr of
	[]	-> y
	x : xs	-> x `deepSeqArray` xs `deepSeqArrays` y

-- Regions --------------------------------------------------------------------
-- | Ensure a region's structure is fully evaluated.
infixr 0 `deepSeqRegion` 
deepSeqRegion :: Shape sh => Region sh a -> b -> b
{-# INLINE deepSeqRegion #-}
deepSeqRegion (Region range gen) x
	= range `deepSeqRange` gen `deepSeqGen` x

-- | Like `deepSeqRegion` but seqs all the regions in a list.
infixr 0 `deepSeqRegions`
deepSeqRegions :: Shape sh => [Region sh a] -> b -> b
{-# INLINE deepSeqRegions #-}
deepSeqRegions rs y
 = case rs of
	[]		-> y
	[r]	 	-> r  `deepSeqRegion`  y
	[r1, r2]	-> r1 `deepSeqRegion` r2 `deepSeqRegion` y
	rs'		-> deepSeqRegions' rs' y

deepSeqRegions' rs' y
 = case rs' of
	[]	-> y
	x : xs	-> x `deepSeqRegion` xs `deepSeqRegions'` y


-- Ranges ---------------------------------------------------------------------
infixr 0 `deepSeqRange`
deepSeqRange :: Shape sh => Range sh -> b -> b
{-# INLINE deepSeqRange #-}
deepSeqRange range x
 = case range of
	RangeAll		-> x
	RangeRects f rects 	-> f `seq` rects `seq` x


-- Generators -----------------------------------------------------------------
-- | Ensure a Generator's structure is fully evaluated.
infixr 0 `deepSeqGen`
deepSeqGen :: Shape sh => Generator sh a -> b -> b
deepSeqGen gen x
 = case gen of
	GenManifest vec		-> vec `seq` x
	GenDelayed{}		-> x
	GenCursor{}		-> x


-- Predicates -------------------------------------------------------------------------------------
inRange :: Shape sh => Range sh -> sh -> Bool
{-# INLINE inRange #-}
inRange RangeAll _		= True
inRange (RangeRects fn _) ix	= fn ix


-- Singletons -------------------------------------------------------------------------------------
-- | Wrap a scalar into a singleton array.
singleton :: Elt a => a -> Array Z a
{-# INLINE singleton #-}
singleton 	= fromFunction Z . const

-- | Take the scalar value from a singleton array.
toScalar :: Elt a => Array Z a -> a
{-# INLINE toScalar #-}
toScalar arr	= arr ! Z

	

-- Projections ------------------------------------------------------------------------------------
-- | Take the extent of an array.
extent	:: Array sh a -> sh
{-# INLINE extent #-}
extent arr	= arrayExtent arr


-- | Unpack an array into delayed form.
delay 	:: (Shape sh, Elt a) 
	=> Array sh a 
	-> (sh, sh -> a)

{-# INLINE delay #-}	
delay arr@(Array sh _)
	= (sh, (arr !))


-- Indexing ---------------------------------------------------------------------------------------
-- | Get an indexed element from an array.
--   This uses the same level of bounds checking as your Data.Vector installation.
--
(!), index
	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array sh a
	-> sh 
	-> a

{-# INLINE (!) #-}
(!) arr ix = index arr ix

{-# INLINE index #-}
index (Array _ []) _
	= error $ stage ++ ": out of regions when indexing array"

index (Array sh (Region range gen : rs)) ix
 	| inRange range ix
	= case gen of
		GenManifest vec		
		 -> vec V.! (S.toIndex sh ix)

		GenDelayed  getElem
		 -> getElem ix

		GenCursor   makeCursor _ loadElem
		 -> loadElem $ makeCursor ix
		
	| otherwise
	= index (Array sh rs) ix


-- | Get an indexed element from an array.
--   If the element is out of range then `Nothing`.
--   TODO: We should probably also ensure that delaying functions don't get
--         called by out of range indices.
(!?), safeIndex
	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array sh a
	-> sh 
	-> Maybe a

{-# INLINE (!?) #-}
(!?) arr ix = safeIndex arr ix

{-# INLINE safeIndex #-}
safeIndex (Array _ []) _
	= error $ stage ++ ": out of regions when indexing array"

safeIndex (Array sh (Region range gen : rs)) ix
 	| inRange range ix
	= case gen of
		GenManifest vec		
		 -> vec V.!? (S.toIndex sh ix)

		GenDelayed  getElem
		 -> Just $ getElem ix

		GenCursor   makeCursor _ loadElem
		 -> Just $ loadElem $ makeCursor ix
		
	| otherwise
	= safeIndex (Array sh rs) ix


-- | Get an indexed element from an array, without bounds checking.
unsafeIndex
	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array sh a
	-> sh 
	-> a

{-# INLINE unsafeIndex#-}
unsafeIndex (Array _ []) _
	= error $ stage ++ ": out of regions when indexing array"

unsafeIndex (Array sh (Region range gen : rs)) ix
 	| inRange range ix
	= case gen of
		GenManifest vec		
		 -> vec `V.unsafeIndex` (S.toIndex sh ix)

		GenDelayed  getElem
		 -> getElem ix

		GenCursor   makeCursor _ loadElem
		 -> loadElem $ makeCursor ix
		
	| otherwise
	= unsafeIndex (Array sh rs) ix


-- Conversions ------------------------------------------------------------------------------------
-- | Create a `Delayed` array from a function.
fromFunction 
	:: Shape sh
	=> sh
	-> (sh -> a)
	-> Array sh a
	
{-# INLINE fromFunction #-}
fromFunction sh fnElems
	= sh `S.deepSeq`
	  Array sh [Region RangeAll (GenDelayed fnElems)]

-- | Create a `Manifest` array from an unboxed `U.Array`. 
--	The elements are in row-major order.
fromVector
	:: Shape sh
	=> sh
	-> Vector a
	-> Array sh a

{-# INLINE fromVector #-}
fromVector sh vec
	= sh  `S.deepSeq` vec `seq`
	  Array sh [Region RangeAll (GenManifest vec)]


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
	| V.length vec /= S.size sh
	= error $ unlines
	 	[ stage ++ ".fromList: size of array shape does not match size of list"
		, "        size of shape = " ++ (show $ S.size sh) 	++ "\n"
		, "        size of list  = " ++ (show $ V.length vec) 	++ "\n" ]
	
	| otherwise
	= Array sh [Region RangeAll (GenManifest vec)]

	where	vec	= V.fromList xx

