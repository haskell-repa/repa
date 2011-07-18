{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Repa.Properties
	( props_DataArrayRepaIndex
	, props_DataArrayRepa)
where
import Data.Array.Repa			as R
import Data.Array.Repa.Arbitrary
import qualified Data.Array.Repa.Shape	as S
import qualified Data.Vector.Unboxed    as V
import Control.Monad
import Test.QuickCheck
import Prelude				as P hiding (compare)

stage   :: String
stage	= "Data.Array.Repa.Properties"

compare :: (Eq a, Show a) => a -> a -> Property
compare ans ref = printTestCase message (ref == ans)
  where
    message = unlines ["*** Expected:", show ref
                      ,"*** Received:", show ans ]


-- Data.Array.Repa.Index --------------------------------------------------------------------------
-- | QuickCheck properties for "Data.Array.Repa.Index".
props_DataArrayRepaIndex :: [(String, Property)]
props_DataArrayRepaIndex
  = [(stage P.++ "." P.++ name, test) | (name, test)
     <-	[ ("toIndexFromIndex/DIM1", 	property prop_toIndexFromIndex_DIM1)
	, ("toIndexFromIndex/DIM2", 	property prop_toIndexFromIndex_DIM2) ]]

prop_toIndexFromIndex_DIM1 sh ix
	=   (sizeIsValid sh)
	==> (inShape sh ix)
	==> fromIndex sh (toIndex sh ix) `compare` (ix :: DIM1)

prop_toIndexFromIndex_DIM2
 =	forAll arbitraryShape   $ \(sh :: DIM2) ->
   	forAll (genInShape2 sh) $ \(ix :: DIM2) ->
	fromIndex sh (toIndex sh ix) `compare` ix




-- Data.Array.Repa --------------------------------------------------------------------------------
-- | QuickCheck properties for "Data.Array.Repa" and its children.
props_DataArrayRepa :: [(String, Property)]
props_DataArrayRepa
 =    props_DataArrayRepaIndex
 P.++ [(stage P.++ "." P.++ name, test) | (name, test)
    <-	[ ("id_force/DIM5",			property prop_id_force_DIM5)
	, ("id_toScalarUnit",			property prop_id_toScalarUnit)
	, ("id_toListFromList/DIM3",		property prop_id_toListFromList_DIM3)
	, ("id_transpose/DIM4",			property prop_id_transpose_DIM4)
	, ("reshapeTransposeSize/DIM3",		property prop_reshapeTranspose_DIM3)
	, ("appendIsAppend/DIM3",		property prop_appendIsAppend_DIM3)
	, ("sumIsSum/DIM3",			property prop_sumIsSum_DIM3)
	, ("sumAllIsSum/DIM3",			property prop_sumAllIsSum_DIM3) ]]


-- The Eq instance uses fold and zipWith.
prop_id_force_DIM5
 = 	forAll (arbitrarySmallArray 10)			$ \(arr :: Array DIM5 Int) ->
	force arr `compare` arr

prop_id_toScalarUnit (x :: Int)
 =	toScalar (singleton x) `compare` x

-- Conversions ------------------------
prop_id_toListFromList_DIM3
 =	forAll (arbitrarySmallShape 10)			$ \(sh :: DIM3) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) ->
	toList (fromList sh xx) `compare` xx

-- Index Space Transforms -------------
prop_id_transpose_DIM4
 = 	forAll (arbitrarySmallArray 20)			$ \(arr :: Array DIM3 Int) ->
	transpose (transpose arr) `compare` arr

-- A reshaped array has the same size and sum as the original
prop_reshapeTranspose_DIM3
 = 	forAll (arbitrarySmallArray 20)			$ \(arr :: Array DIM3 Int) ->
   let	arr'	= transpose arr
   	sh'	= extent arr'
   in	(S.size (extent (reshape sh' arr)) `compare` S.size (extent arr))
   .&&. (sumAll arr'                       `compare` sumAll arr)

prop_appendIsAppend_DIM3
 = 	forAll (arbitrarySmallArray 20)			$ \(arr1 :: Array DIM3 Int) ->
	sumAll (append arr1 arr1) `compare` (2 * sumAll arr1)

-- Reductions --------------------------
prop_sumIsSum_DIM3
  = forAll (arbitrarySmallArray 20)                     $ \(arr :: Array DIM3 Int) ->
    let sh :. sz  = extent arr
        elemFn ix = V.foldl' (+) 0
                  $ V.map (\i -> arr ! (ix :. i))
                          (V.enumFromTo 0 (sz-1))
    in
    R.fold (+) 0 arr `compare` fromFunction sh elemFn

prop_sumAllIsSum_DIM3
 = 	forAll (arbitrarySmallShape 20)		        $ \(sh :: DIM3) ->
	forAll (arbitraryListOfLength (S.size sh))	$ \(xx :: [Int]) ->
        sumAll (fromList sh xx) `compare` P.sum xx


-- Utils ------------------------------------------------------------------------------------------
genInShape2 :: DIM2 -> Gen DIM2
genInShape2 (Z :. yMax :. xMax)
 = do	y	<- liftM (`mod` yMax) $ arbitrary
	x	<- liftM (`mod` xMax) $ arbitrary
	return	$ Z :. y :. x

