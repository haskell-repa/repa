{-# OPTIONS -fglasgow-exts #-}
module CArrayFlatDim
	( Array 	(..)
	, CArray	(..)
	, (!:)
	, takeCArrayOfArray
	, takeArrayOfCArray
	, forceCArray 
	, zipWith)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted.Gabi	(mapU, foldU, enumFromToU)
import Array					(Array(..))
import Prelude 					hiding (map, zip, zipWith, replicate, sum)
import FlatDim
import Data.Maybe
import Data.Either
import GHC.Exts


-- CArray (cache array) --------------------------------------------------------------------------
data CArray dim e 
	= CArray
	{ carrayShape	:: dim
	, carrayCache	:: Either (dim -> e) (U.Array e) }
	

-- Primitive functions ----------------------------------------------------------------------------
-- | Lookup the value in an array.
(!:) 	:: (FShape dim, U.Elt e)
	=> CArray dim e -> dim -> e

{-# INLINE (!:) #-}
(!:) arr ix
 = case carrayCache arr of
	Right uarr	-> uarr U.!: (toIndex (carrayShape arr) ix)
	Left  fn	-> fn ix
	
	
-- Conversion -------------------------------------------------------------------------------------
-- | Convert a strict array into a cached array.
takeCArrayOfArray :: (U.Elt e, FShape dim) => Array dim e -> CArray dim e
{-# INLINE takeCArrayOfArray #-}
takeCArrayOfArray arr
 	=     arrayShape arr 
	`seq` arrayData arr 
	`seq` CArray
		{ carrayShape	= arrayShape arr
		, carrayCache	= Right (arrayData arr) }
		

-- | Convert a cache array into a strict array
takeArrayOfCArray :: (U.Elt e, FShape dim) => CArray dim e -> Array dim e
{-# INLINE takeArrayOfCArray #-}
takeArrayOfCArray arr	
 	= Array
	{ arrayData
		= case carrayCache arr of
			Left fn
			 -> U.map (fn . fromIndex (carrayShape arr))
			  $ U.enumFromTo 
				(0 :: Int)
				((size $ carrayShape arr) - 1)
				
			Right uarr -> uarr
			
	, arrayShape
		= carrayShape arr }
		

-- Forcing ----------------------------------------------------------------------------------------
forceCArray 
	:: (U.Elt e, FShape dim) 
	=> CArray dim e
	-> CArray dim e

{-# INLINE forceCArray #-}
forceCArray carr
 = let	farr	= takeArrayOfCArray carr
   in	arrayData farr `seq` (takeCArrayOfArray farr)


-- Computations -----------------------------------------------------------------------------------
-- | If the size of two array arguments differ in a dimension, the resulting
--   array's shape is the minimum of the two 
zipWith :: (U.Elt a, U.Elt b, U.Elt c, FShape dim) 
	=> (a -> b -> c) 
	-> CArray dim a
	-> CArray dim b
	-> CArray dim c

{-# INLINE zipWith #-}
zipWith f arr1 arr2
	= CArray (carrayShape arr1) 
		{-(A.intersectDim 		-- TODO: intersection on FDIM isn't implemented yet.
			(carrayShape arr1)
			(carrayShape arr2)) -}
		 (Left (\i -> f (arr1 !: i) (arr2 !: i)))

		

