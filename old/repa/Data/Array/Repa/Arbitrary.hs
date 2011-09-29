{-# LANGUAGE TypeOperators, FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

-- Utils to help with testing. Not exported.
module Data.Array.Repa.Arbitrary
	( arbitraryShape
	, arbitrarySmallShape
	, arbitraryListOfLength
	, arbitrarySmallArray)
where
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Shape	as S
import Control.Monad
import Test.QuickCheck


-- Arbitrary --------------------------------------------------------------------------------------
instance Arbitrary Z where
	arbitrary	= return Z

-- | Generate an arbitrary index, which may have 0's for some components.
instance (Shape sh, Arbitrary sh) => Arbitrary (sh :. Int)  where
	arbitrary
	 = do	sh1		<- arbitrary
		let sh1Unit	= if size sh1 == 0 then unitDim else sh1

		-- Make sure not to create an index so big that we get
		--	integer overflow when converting it to the linear form.
		n		<- liftM abs $ arbitrary
		let nMax	= maxBound `div` (size sh1Unit)
		let nMaxed	= n `mod` nMax

		return	$ sh1 :. nMaxed

-- | Generate an aribrary shape that does not have 0's for any component.
arbitraryShape
	:: (Shape sh, Arbitrary sh)
	=> Gen (sh :. Int)

arbitraryShape
 = do	sh1		<- arbitrary
	let sh1Unit	= if size sh1 == 0 then unitDim else sh1

	-- Make sure not to create an index so big that we get
	--	integer overflow when converting it to the linear form.
	n		<- liftM abs $ arbitrary
	let nMax	= maxBound `div` size sh1Unit
	let nMaxed	= n `mod` nMax
	let nClamped	= if nMaxed == 0 then 1 else nMaxed

	return $ sh1Unit :. nClamped


-- | Generate an arbitrary shape where each dimension is more than zero,
--	but less than a specific value.
arbitrarySmallShape
	:: (Shape sh, Arbitrary sh)
	=> Int
	-> Gen (sh :. Int)

arbitrarySmallShape maxDim
 = do	sh		<- arbitraryShape
	let dims	= listOfShape sh

	let clamp x
		= case x `mod` maxDim of
			0	-> 1
			n	-> n

	return	$ if True
			then shapeOfList $ map clamp dims
			else sh


arbitraryListOfLength
	:: Arbitrary a
	=> Int -> Gen [a]

arbitraryListOfLength n
	| n == 0		= return []
	| otherwise
	= do	i	<- arbitrary
		rest	<- arbitraryListOfLength (n - 1)
		return	$ i : rest

-- | Create an arbitrary small array, restricting the size of each of the
--   dimensions to some value.
arbitrarySmallArray
	:: (Shape sh, Elt a, Arbitrary sh, Arbitrary a)
	=> Int
	-> Gen (Array (sh :. Int) a)

arbitrarySmallArray maxDim
 = do	sh	<- arbitrarySmallShape maxDim
	xx	<- arbitraryListOfLength (S.size sh)
	return	$ fromList sh xx

