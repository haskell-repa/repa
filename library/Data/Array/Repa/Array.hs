{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Repa.Array
	( Array	(..)
	, force
	, fromList
	, toList
	, tests_DataArrayRepaArray)
where
import qualified Data.Array.Repa.Shape		as S
import Data.Array.Repa.Index
import Data.Array.Repa.Shape			(Shape)
import Data.Array.Repa.QuickCheck
import Test.QuickCheck
import qualified Data.Array.Parallel.Unlifted	as U
	
stage	= "Data.Array.Repa.Array"
	
	
-- | Possibly delayed arrays.
data Array sh e 
	= Manifest sh (U.Array e)
	| Delayed  sh (sh -> e)


-- | Force an array, so that it becomes `Manifest`.
force
	:: (Shape sh, U.Elt e)
	=> Array sh e
	-> Array sh e
	
force arr@Manifest{}	
	= arr

force (Delayed sh fn)
 	= Manifest sh
	$ U.map (fn . S.fromIndex sh)
	$ U.enumFromTo 
		(0 :: Int)
		(S.size sh - 1)


-- | Convert a list to an `Array`.
fromList 
	:: (Shape sh, U.Elt e)
	=> sh
	-> [e]
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
toList 	:: (Shape sh, U.Elt e)
	=> Array sh e
	-> [e]

toList arr
 = case force arr of
	Manifest _ uarr	-> U.toList uarr
	_		-> error $ stage ++ ".toList: force failed"



-- Tests ------------------------------------------------------------------------------------------
tests_DataArrayRepaArray
 = 	[ ("toListFromList/DIM3",	property prop_toListFromList_DIM3) ]

prop_toListFromList_DIM3
 = 	forAll (arbitrarySmallShape 10)			$ \(sh :: DIM3) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) ->
	toList (fromList sh xx) == xx
	

