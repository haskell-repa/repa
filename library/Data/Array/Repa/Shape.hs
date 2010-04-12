{-# LANGUAGE TypeOperators, FlexibleInstances #-}

-- | Class of types that can be used as array shapes and indices.
module Data.Array.Repa.Shape
	( Shape(..)
	, tests_DataArrayRepaShape)
where
import Data.Array.Repa.Index
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import GHC.Base 				(quotInt, remInt)
	

-- Shape ------------------------------------------------------------------------------------------	
-- | Class of types that can be used as array shapes and indices.
class (Show sh, Eq sh) => Shape sh where

	-- | Get the number of dimensions in a `Shape`.
	dim	:: sh -> Int           

	-- | The shape of an array of size zero, with a particular dimensionality.
	zeroDim	:: sh

	-- | Get the total number of elements in an array with this `Shape`.
	size	:: sh -> Int           

	-- | Compute an index into the linear representation of an array.
	toIndex :: sh	-- ^ Shape of the array.
		-> sh 	-- ^ Index into the array.
		-> Int     

	-- | Given an index into the linear row-major representation of an
	--	array, calculate the shape index into the Array.
	fromIndex 
		:: sh 	-- ^ Shape of the array.
		-> Int 	-- ^ Index into linear representation.
		-> sh   

	-- | Check whether a given index is in the range of an array shape. 
	--   The following property holds:
	--
	--	@inRange sh ind == elem ind (range sh)@
	inRange	:: sh 	-- ^ Start index for range.
		-> sh 	-- ^ Final index for range.
		-> sh 	-- ^ Index to check for.
		-> Bool

	-- | Compute the intersection of two shapes.
	intersectDim :: sh -> sh -> sh

	-- | What does this do?
	next	:: sh -> sh -> Maybe sh

	-- | Ensure that a `Shape` is completely evaluated.
	infixr 0 `deepSeq`
	deepSeq :: sh -> a -> a


-- Instances --------------------------------------------------------------------------------------
instance Shape () where
	dim _			= 0
	size _			= 1
	toIndex _ _		= 0
	fromIndex _ _		= ()

	inRange () () ()	= True
	zeroDim			= ()
	intersectDim _ _	= ()
	next _ _		= Nothing

	deepSeq () x		= x


instance Shape sh => Shape (sh :. Int) where
	{-# INLINE dim #-}
	dim   (sh  :. _)
		= dim sh + 1

	{-# INLINE size #-}
	size  (sh1 :. n)
		= size sh1 * n

	{-# INLINE toIndex #-}
	toIndex (sh1 :. sh2) (sh1' :. sh2') 
		= toIndex sh1 sh1' * sh2 + sh2'

	{-# INLINE fromIndex #-}
	fromIndex (ds :. d) n 
	 	= fromIndex ds (n `quotInt` d) :. r
		where
		-- If we assume that the index is in range, there is no point
		-- in computing the remainder for the highest dimension since
		-- n < d must hold. This saves one remInt per element access which
		-- is quite a big deal.
		r 	| dim ds == 0	= n
			| otherwise	= n `remInt` d

	{-# INLINE inRange #-}
	inRange (zs :. z) (sh1 :. n1) (sh2 :. n2) 
		= (n2 >= z) && (n2 < n1) && (inRange zs sh1 sh2)

	{-# INLINE zeroDim #-}
	zeroDim = (zeroDim :. 0)

	{-# INLINE intersectDim #-}
	intersectDim (sh1 :. n1) (sh2 :. n2) 
		= (intersectDim sh1 sh2 :. (min n1 n2))

	{-# INLINE next #-}
	next  sh@(sh' :. s) msh@(msh' :. ms) 
		| sh == msh     = Nothing
		| s  < (ms-1)   = Just (sh' :. (s+1))    
		| otherwise 
		= case next sh' msh' of
			Just shNext -> Just (shNext :. 0)
			Nothing     -> Nothing
           
	{-# INLINE deepSeq #-} 
	deepSeq (sh :. n) x = deepSeq sh (n `seq` x)


-- Tests ------------------------------------------------------------------------------------------
tests_DataArrayRepaShape
	= testGroup "Data.Array.Repa.Shape"
		[ testProperty "toIndexFromIndex" prop_Shape2_toIndexFromIndex ]

prop_Shape2_toIndexFromIndex sh ix
	= fromIndex sh (toIndex sh ix) == ix
	where	_types	= ( sh :: (() :. Int :. Int)
			  , ix :: (() :. Int :. Int))
		
		

