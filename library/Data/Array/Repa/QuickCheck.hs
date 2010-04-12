
-- Utils to help with testing. Not exported.
module Data.Array.Repa.QuickCheck
	(arbitraryListOfLength)
where
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
	
	
	
