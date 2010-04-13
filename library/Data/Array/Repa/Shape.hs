{-# LANGUAGE TypeOperators, FlexibleInstances #-}

-- | Class of types that can be used as array shapes and indices.
module Data.Array.Repa.Shape
	(Shape(..))
where
import Test.QuickCheck.Arbitrary
	
-- Shape ------------------------------------------------------------------------------------------	
-- | Class of types that can be used as array shapes and indices.
class (Eq sh, Arbitrary sh) => Shape sh where

	-- | Get the number of dimensions in a `Shape`.
	dim	:: sh -> Int           

	-- | The shape of an array of size zero, with a particular dimensionality.
	zeroDim	:: sh

	-- | The shape of an array with size one, with a particular dimensionality.
	unitDim :: sh

	-- | Compute the intersection of two shapes.
	intersectDim :: sh -> sh -> sh


	-- | Get the total number of elements in an array with this `Shape`.
	size	:: sh -> Int           

	-- | Check whether the size of this shape is small enough to be represented
	--	as an integer index.
	sizeIsValid :: sh -> Bool


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
	inRange	:: sh 	-- ^ Start index for range.
		-> sh 	-- ^ Final index for range.
		-> sh 	-- ^ Index to check for.
		-> Bool


	-- | ??? What is this for?
	next	:: sh -> sh -> Maybe sh

	-- | Ensure that a `Shape` is completely evaluated.
	infixr 0 `deepSeq`
	deepSeq :: sh -> a -> a


	-- | Convert a shape to a list of dimensions.
	listOfShape	:: sh -> [Int]
	
	-- | Convert a list of dimensions to a shape
	shapeOfList	:: [Int] -> sh
	

	-- | Check whether an index is a part of a given shape.
	inShape :: sh	-- ^ Shape.
		-> sh	-- ^ Index.
		-> Bool

	inShape sh ix
		= inRange zeroDim sh ix

