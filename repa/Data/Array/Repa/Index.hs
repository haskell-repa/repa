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
	, DIM5 
	
	-- * Testing
	, arbitraryShape
	, arbitrarySmallShape
	, props_DataArrayRepaIndex)
where
import Data.Array.Repa.Shape
import Test.QuickCheck
import Control.Monad
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

	{-# INLINE size #-}
	size _			= 1

	{-# INLINE sizeIsValid #-}
	sizeIsValid _		= True


	{-# INLINE toIndex #-}
	toIndex _ _		= 0

	{-# INLINE fromIndex #-}
	fromIndex _ _		= Z


	{-# INLINE inRange #-}
	inRange Z Z Z		= True

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

	{-# INLINE inRange #-}
	inRange (zs :. z) (sh1 :. n1) (sh2 :. n2) 
		= (n2 >= z) && (n2 < n1) && (inRange zs sh1 sh2)


       	listOfShape (sh :. n)
	 = n : listOfShape sh

	shapeOfList xx
	 = case xx of
		[]	-> error $ stage ++ ".toList: empty list when converting to  (_ :. Int)"
		x:xs	-> shapeOfList xs :. x			

	{-# INLINE deepSeq #-} 
	deepSeq (sh :. n) x = deepSeq sh (n `seq` x)




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


genInShape2 :: DIM2 -> Gen DIM2
genInShape2 (Z :. yMax :. xMax)
 = do	y	<- liftM (`mod` yMax) $ arbitrary
	x	<- liftM (`mod` xMax) $ arbitrary
	return	$ Z :. y :. x


-- Properties -------------------------------------------------------------------------------------
-- | QuickCheck properties for this module.
props_DataArrayRepaIndex :: [(String, Property)]
props_DataArrayRepaIndex
  = [(stage ++ "." ++ name, test) | (name, test)
     <-	[ ("toIndexFromIndex/DIM1", 	property prop_toIndexFromIndex_DIM1) 
	, ("toIndexFromIndex/DIM2", 	property prop_toIndexFromIndex_DIM2) ]]

prop_toIndexFromIndex_DIM1 sh ix
	=   (sizeIsValid sh)
	==> (inShape sh ix)
	==> fromIndex sh (toIndex sh ix) == ix
	where	_types	= ( sh :: DIM1
			  , ix :: DIM1)

prop_toIndexFromIndex_DIM2
 =	forAll arbitraryShape   $ \(sh :: DIM2) ->
   	forAll (genInShape2 sh) $ \(ix :: DIM2) ->
	fromIndex sh (toIndex sh ix) == ix



