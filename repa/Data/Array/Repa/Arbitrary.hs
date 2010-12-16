{-# LANGUAGE TypeOperators #-}

-- Utils to help with testing. Not exported.
module Data.Array.Repa.Arbitrary
	( arbitraryListOfLength
	, arbitrarySmallArray)
	
where
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Shape	as S
import Test.QuickCheck

	
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

