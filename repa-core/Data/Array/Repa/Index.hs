
-- | Index types.
module Data.Array.Repa.Index
	(
	-- * Index types
	  Z	(..)
	, (:.)	(..)

	-- * Common dimensions.
	, DIM0, DIM1, DIM2, DIM3, DIM4, DIM5
        ,       ix1,  ix2,  ix3,  ix4,  ix5)
where
import Data.Array.Repa.Shape
import GHC.Base 		(quotInt, remInt)


-- | An index of dimension zero
data Z	= Z
	deriving (Show, Read, Eq, Ord)

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head
	= !tail :. !head
	deriving (Show, Read, Eq, Ord)


-- Common dimensions
type DIM0	= Z
type DIM1	= DIM0 :. Int
type DIM2	= DIM1 :. Int
type DIM3	= DIM2 :. Int
type DIM4	= DIM3 :. Int
type DIM5	= DIM4 :. Int


-- | Helper for index construction, which constrains the coordinate to be 
--   an @Int@.
ix1 :: Int -> DIM1
ix1 x = Z :. x
{-# INLINE ix1 #-}

ix2 :: Int -> Int -> DIM2
ix2 y x = Z :. y :. x
{-# INLINE ix2 #-}

ix3 :: Int -> Int -> Int -> DIM3
ix3 z y x = Z :. z :. y :. x
{-# INLINE ix3 #-}

ix4 :: Int -> Int -> Int -> Int -> DIM4
ix4 a z y x = Z :. a :. z :. y :. x
{-# INLINE ix4 #-}

ix5 :: Int -> Int -> Int -> Int -> Int -> DIM5
ix5 b a z y x = Z :. b :. a :. z :. y :. x
{-# INLINE ix5 #-}


-- Shape ----------------------------------------------------------------------
instance Shape Z where
	rank _			= 0
        {-# INLINE [1] rank #-}

	zeroDim		 	= Z
        {-# INLINE [1] zeroDim #-}

	unitDim			= Z
        {-# INLINE [1] unitDim #-}

	intersectDim _ _	= Z
        {-# INLINE [1] intersectDim #-}

	addDim _ _		= Z
        {-# INLINE [1] addDim #-}

	size _			= 1
        {-# INLINE [1] size #-}

	toIndex _ _		= 0
        {-# INLINE [1] toIndex #-}

	fromIndex _ _		= Z
        {-# INLINE [1] fromIndex #-}


	inShapeRange Z Z Z	= True
        {-# INLINE [1] inShapeRange #-}

	listOfShape _		= []
        {-# NOINLINE listOfShape #-}

	shapeOfList []		= Just Z
	shapeOfList _		= Nothing
        {-# NOINLINE shapeOfList #-}



instance Shape sh => Shape (sh :. Int) where
	rank   (sh  :. _)
		= rank sh + 1
        {-# INLINE [1] rank #-}

	zeroDim = zeroDim :. 0
        {-# INLINE [1] zeroDim #-}

	unitDim = unitDim :. 1
        {-# INLINE [1] unitDim #-}

	intersectDim (sh1 :. n1) (sh2 :. n2)
		= (intersectDim sh1 sh2 :. (min n1 n2))
        {-# INLINE [1] intersectDim #-}

	addDim (sh1 :. n1) (sh2 :. n2)
		= addDim sh1 sh2 :. (n1 + n2)
        {-# INLINE [1] addDim #-}

	size  (sh1 :. n)
		= size sh1 * n
        {-# INLINE [1] size #-}

	toIndex (sh1 :. sh2) (sh1' :. sh2')
		= toIndex sh1 sh1' * sh2 + sh2'
        {-# INLINE [1] toIndex #-}

        fromIndex (ds :. d) n
                = fromIndex ds (n `quotInt` d) :. r
                where
                -- If we assume that the index is in range, there is no point
                -- in computing the remainder for the highest dimension since
                -- n < d must hold. This saves one remInt per element access which
                -- is quite a big deal.
                r       | rank ds == 0  = n
                        | otherwise     = n `remInt` d
        {-# INLINE [1] fromIndex #-}

	inShapeRange (zs :. z) (sh1 :. n1) (sh2 :. n2)
		= (n2 >= z) && (n2 < n1) && (inShapeRange zs sh1 sh2)
        {-# INLINE [1] inShapeRange #-}

       	listOfShape (sh :. n)
	 = n : listOfShape sh
        {-# NOINLINE listOfShape #-}

	shapeOfList xx
	 = case xx of
		[]	-> Nothing
		x : xs	-> do ss <- shapeOfList xs 
                              return $ ss :. x
        {-# NOINLINE shapeOfList #-}


