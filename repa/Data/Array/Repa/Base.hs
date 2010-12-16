{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ExplicitForAll, FlexibleInstances, UndecidableInstances #-}

module Data.Array.Repa.Base
	( Elt
	, Array(..)
	, deepSeqArray
	, singleton, toScalar
	, extent,    delay

	-- * Indexing
	, (!),  index
	, (!?), safeIndex
	, unsafeIndex

	-- * Conversions 
	, fromFunction	
	, fromVector, toVector
	, fromList,   toList
	
	-- * Forcing
	, force)
where
import Data.Array.Repa.Index
import Data.Array.Repa.Internals.Evaluate
import Data.Array.Repa.Shape			as S
import qualified Data.Vector.Unboxed		as V
import Data.Vector.Unboxed			(Vector)

stage	= "Data.Array.Repa.Array"

-- Array -----------------------------------------------------------------------------------------	
-- | Possibly delayed arrays.
data Array sh a
	= -- | An array represented as some concrete unboxed data.
	  Manifest !sh !(Vector a)

          -- | An array represented as a function that computes each element.
	| Delayed  !sh !(sh -> a)


-- | Ensure an array's structure is fully evaluated.
--   This evaluates the extent and outer constructor, but does not `force` the elements.
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
	Delayed  _ fn		-> fn Z
	Manifest _ uarr		-> uarr V.! 0


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
	Delayed  sh fn	-> (sh, fn)
	Manifest sh vec	-> (sh, \ix -> vec V.! S.toIndex sh ix)


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
	Delayed  _  fn		-> fn ix
	Manifest sh vec		-> vec V.! (S.toIndex sh ix)


-- | Get an indexed element from an array.
--   If the element is out of range then `Nothing`.
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
	Delayed  _  fn		-> Just (fn ix)
	Manifest sh vec		-> vec V.!? (S.toIndex sh ix)


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
	Delayed  _  fn		-> fn ix
	Manifest sh uarr	-> uarr `V.unsafeIndex` (S.toIndex sh ix)



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


-- | Convert an array to an unboxed `U.Array`, forcing it if required.
--	The elements come out in row-major order.
toVector
	:: (Shape sh, Elt a)
	=> Array sh a 
	-> Vector a
{-# INLINE toVector #-}
toVector arr
 = case force arr of
	Manifest _ vec	-> vec
	_		-> error $ stage ++ ".toVector: force failed"


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
	
-- | Convert an array to a list.
toList 	:: (Shape sh, Elt a)
	=> Array sh a
	-> [a]

{-# INLINE toList #-}
toList arr
 = case force arr of
	Manifest _ vec	-> V.toList vec
	_		-> error $ stage ++ ".toList: force failed"

-- Forcing ----------------------------------------------------------------------------------------
-- | Force an array, so that it becomes `Manifest`.
--   This evaluates all elements in parallel.
force	:: (Shape sh, Elt a)
	=> Array sh a -> Array sh a
	
{-# INLINE force #-}
force arr
 = Manifest sh' vec'
 where	(sh', vec')
	 = case arr of
		Manifest sh vec
		 -> sh `S.deepSeq` vec `seq` (sh, vec)
		
		Delayed sh getElem
		 -> let vec	= evalParLinear sh (getElem . fromIndex sh)
		    in  sh `S.deepSeq` vec `seq` (sh, vec)

{-
-- TODO: make this parallel again
--	change V.map and V.enumFromTo
force arr

 = Manifest sh' uarr'
 where (sh', uarr')
	= case arr of
		Manifest sh uarr
	 	 -> sh `S.deepSeq` uarr `seq` (sh, uarr)

		Delayed sh fn
	 	 -> let uarr	=  V.map (fn . S.fromIndex sh) 
				$! V.enumFromTo (0 :: Int) (S.size sh - 1)
	    	     in	sh `S.deepSeq` uarr `seq` (sh, uarr)
-}



-- Elements ---------------------------------------------------------------------------------------
class V.Unbox a	=> Elt a

instance V.Unbox a => Elt a



