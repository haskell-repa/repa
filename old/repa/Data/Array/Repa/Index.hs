{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}

-- | Index types.
module Data.Array.Repa.Index
	(
	-- * Index types
	  Z	(..)
	, (:.)	(..)

	-- * Common dimensions.
	, DIM0
	, DIM1
	, DIM2
	, DIM3
	, DIM4
	, DIM5)
where
import Data.Array.Repa.Shape
import GHC.Base 		(quotInt, remInt)

stage	= "Data.Array.Repa.Index"

-- | An index of dimension zero
data Z	= Z
	deriving (Show, Eq, Ord)

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head
	= tail :. head
	deriving (Show, Eq, Ord)

-- Common dimensions
type DIM0	= Z
type DIM1	= DIM0 :. Int
type DIM2	= DIM1 :. Int
type DIM3	= DIM2 :. Int
type DIM4	= DIM3 :. Int
type DIM5	= DIM4 :. Int


-- Shape ------------------------------------------------------------------------------------------
instance Shape Z where
	{-# INLINE rank #-}
	rank _			= 0

	{-# INLINE zeroDim #-}
	zeroDim			= Z

	{-# INLINE unitDim #-}
	unitDim			= Z

	{-# INLINE intersectDim #-}
	intersectDim _ _	= Z

	{-# INLINE addDim #-}
	addDim _ _		= Z

	{-# INLINE size #-}
	size _			= 1

	{-# INLINE sizeIsValid #-}
	sizeIsValid _		= True


	{-# INLINE toIndex #-}
	toIndex _ _		= 0

	{-# INLINE fromIndex #-}
	fromIndex _ _		= Z


	{-# INLINE inShapeRange #-}
	inShapeRange Z Z Z	= True

	listOfShape _		= []
	shapeOfList []		= Z
	shapeOfList _		= error $ stage ++ ".fromList: non-empty list when converting to Z."

	{-# INLINE deepSeq #-}
	deepSeq Z x		= x


instance Shape sh => Shape (sh :. Int) where
	{-# INLINE rank #-}
	rank   (sh  :. _)
		= rank sh + 1

	{-# INLINE zeroDim #-}
	zeroDim = zeroDim :. 0

	{-# INLINE unitDim #-}
	unitDim = unitDim :. 1

	{-# INLINE intersectDim #-}
	intersectDim (sh1 :. n1) (sh2 :. n2)
		= (intersectDim sh1 sh2 :. (min n1 n2))

	{-# INLINE addDim #-}
	addDim (sh1 :. n1) (sh2 :. n2)
		= addDim sh1 sh2 :. (n1 + n2)

	{-# INLINE size #-}
	size  (sh1 :. n)
		= size sh1 * n

	{-# INLINE sizeIsValid #-}
	sizeIsValid (sh1 :. n)
		| size sh1 > 0
		= n <= maxBound `div` size sh1

		| otherwise
		= False

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
		r 	| rank ds == 0	= n
			| otherwise	= n `remInt` d

	{-# INLINE inShapeRange #-}
	inShapeRange (zs :. z) (sh1 :. n1) (sh2 :. n2)
		= (n2 >= z) && (n2 < n1) && (inShapeRange zs sh1 sh2)


       	listOfShape (sh :. n)
	 = n : listOfShape sh

	shapeOfList xx
	 = case xx of
		[]	-> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
		x:xs	-> shapeOfList xs :. x

	{-# INLINE deepSeq #-}
	deepSeq (sh :. n) x = deepSeq sh (n `seq` x)

