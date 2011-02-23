{-# LANGUAGE ExplicitForAll, TypeOperators, FlexibleInstances, UndecidableInstances, BangPatterns,
             ExistentialQuantification #-}
module Data.Array.Repa.Internals.Base
	( Array(..)
	, Cursor(..)
	, deepSeqArray, deepSeqArrays
	, singleton, toScalar
	, extent,    delay

	-- * Predicates
	, isManifest
	, isDelayed
	, isDelayedCursor
	, isPartitioned

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

-- | A index into the flat array.
--   Should be abstract outside the stencil modules.
data Cursor 
	= Cursor Int

-- Array -----------------------------------------------------------------------------------------	
-- | Possibly delayed arrays.
data Array sh a
	= -- | An array represented as some concrete unboxed data.
	  Manifest  
		{ arrayExtent		:: sh 
		, arrayVector		:: (Vector a) }

          -- | An array represented as a function that computes each element.
	| Delayed 
		{ arrayExtent		:: sh
		, arrayGetElem		:: (sh -> a) }

	  -- Try a version with cursors
	| forall cursor
	. DelayedCursor
		{ arrayExtent		:: sh
		, arrayMakeCursor	:: sh -> cursor
		, arrayShiftCursor	:: sh -> cursor -> cursor
		, arrayLoadElem		:: cursor -> a }

	  -- | An delayed array broken into subranges.
	  --   INVARIANT: the ranges to not overlap.
	  --   INVARIANT: for a singleton array both elem fns return the same result.
	  --   TODO:      Try to store the ranges in a vector. We might need more instances.
	| Partitioned 
		{ arrayExtent		:: sh		-- extent of whole array.
		, arrayChoose		:: (sh -> Bool)	-- fn to decide if we're in the first or second segment.
		, arrayBorderRanges	:: [(sh, sh)] 
		, arrayBorderGetElem	:: (sh -> a)	-- if we're in any of these ranges then use this fn.
		, arrayInnerRange	:: (sh, sh)
		, arrayInnerGetElem	:: (sh -> a) }	--   otherwise use this other one.

{-	Maybe move to this version instead of partitioned, 
	so we can have any number of partitions.
	| Region 
		{ arrayRange		:: (sh, sh)
		, arrayGetElem		:: sh -> a
		, arrayNext		:: Array sh a }
-}


-- | Ensure an array's structure is fully evaluated.
--   This evaluates the extent and outer constructor, but does not `force` the elements.
--   TODO: Force the list in the Partitioned version.
infixr 0 `deepSeqArray`
deepSeqArray 
	:: Shape sh
	=> Array sh a 
	-> b -> b

{-# INLINE deepSeqArray #-}
deepSeqArray arr x 
 = case arr of
	Manifest  sh uarr	-> sh `S.deepSeq` uarr `seq` x
	Delayed   sh _		-> sh `S.deepSeq` x
	DelayedCursor sh _ _ _	-> sh `S.deepSeq` x
	Partitioned sh _ _ _ _ _-> sh `S.deepSeq` x


-- | Like `deepSeqArray` but seqs all the arrays in a list.
infixr 0 `deepSeqArrays`
deepSeqArrays
	:: Shape sh
	=> [Array sh a]
	-> b -> b

{-# INLINE deepSeqArrays #-}
deepSeqArrays arr y
 = case arr of
	[]	-> y
	x : xs	-> x `deepSeqArray` xs `deepSeqArrays` y
	

-- Predicates -------------------------------------------------------------------------------------
isManifest  :: Array sh a -> Bool
isManifest arr
 = case arr of
	Manifest{}	-> True
	_		-> False
	
isDelayed   :: Array sh a -> Bool
isDelayed arr
 = case arr of
	Delayed{}	-> True
	_		-> False

isDelayedCursor :: Array sh a -> Bool
isDelayedCursor arr
 = case arr of
	DelayedCursor{}	-> True
	_		-> False

isPartitioned :: Array sh a -> Bool
isPartitioned arr
 = case arr of
	Partitioned{}	-> True
	_		-> False


-- Singletons -------------------------------------------------------------------------------------
-- | Wrap a scalar into a singleton array.
singleton :: Elt a => a -> Array Z a
{-# INLINE singleton #-}
singleton 	= Delayed Z . const


-- | Take the scalar value from a singleton array.
toScalar :: Elt a => Array Z a -> a
{-# INLINE toScalar #-}
toScalar arr
 = case arr of
	Manifest  _ uarr	-> uarr V.! 0

	Delayed{}
	 -> arrayGetElem arr Z

	DelayedCursor _ makeCursor _ loadElem
	 -> (loadElem . makeCursor) Z

	Partitioned{}
	 -> arrayInnerGetElem arr Z
	

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
delay arr
 = case arr of
	Delayed   sh fn	
	 -> (sh, fn)
	
	DelayedCursor sh makeCursor _ loadElem
	 -> (sh, loadElem . makeCursor)

	Manifest  sh vec
	 -> (sh, \ix -> vec V.! S.toIndex sh ix)

	Partitioned{}
	 -> ( arrayExtent arr
	    , \ix -> if arrayChoose arr ix
			   then arrayBorderGetElem arr ix
			   else arrayInnerGetElem  arr ix)


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
index arr ix
 = case arr of
	Manifest  sh vec
	 -> vec V.! (S.toIndex sh ix)

	Delayed   _  fn
	 -> fn ix

	DelayedCursor _ makeCursor _ loadElem
	 -> loadElem $ makeCursor ix

	Partitioned{}
	 -> if arrayChoose arr ix
		then arrayBorderGetElem arr ix
		else arrayInnerGetElem  arr ix


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
safeIndex arr ix
 = case arr of
	Manifest sh vec		
	 -> vec V.!? (S.toIndex sh ix)

	Delayed  _  fn
	 -> Just (fn ix)

	DelayedCursor _ makeCursor _ loadElem
	 -> Just (loadElem $ makeCursor ix)

	Partitioned{}
	 -> Just (if arrayChoose arr ix
		  	then arrayBorderGetElem arr ix
			else arrayInnerGetElem  arr ix)


-- | Get an indexed element from an array, without bounds checking.
--
--   OBLIGATION: The index must be within the array. 
--
-- 	@inRange zeroDim (shape arr) ix == True@
-- 
unsafeIndex
	:: forall sh a
	.  (Shape sh, Elt a)
	=> Array sh a
	-> sh 
	-> a

{-# INLINE unsafeIndex #-}
unsafeIndex arr ix
 = case arr of
	Manifest sh uarr
	 -> uarr `V.unsafeIndex` (S.toIndex sh ix)

	Delayed  _  fn
	 -> fn ix

	DelayedCursor _ makeCursor _ loadElem
	 -> loadElem $ makeCursor ix

	Partitioned{}
	 -> if arrayChoose arr ix
		then arrayBorderGetElem arr ix
		else arrayInnerGetElem  arr ix


-- Conversions ------------------------------------------------------------------------------------
-- | Create a `Delayed` array from a function.
fromFunction 
	:: Shape sh
	=> sh
	-> (sh -> a)
	-> Array sh a
	
{-# INLINE fromFunction #-}
fromFunction sh fnElems
	= sh `S.deepSeq` Delayed sh fnElems

-- | Create a `Manifest` array from an unboxed `U.Array`. 
--	The elements are in row-major order.
fromVector
	:: Shape sh
	=> sh
	-> Vector a
	-> Array sh a

{-# INLINE fromVector #-}
fromVector sh vec
	= sh   `S.deepSeq` 
	  vec `seq`
	  Manifest sh vec


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
	= Manifest sh vec

	where	vec	= V.fromList xx
	
