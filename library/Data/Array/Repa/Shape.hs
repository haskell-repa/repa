

module Data.Array.Repa.Shape
	(Shape(..))
where
	
	
class (Show sh, Eq sh) => Shape sh where

	-- | Get the number of dimensions in a `Shape`.
	dim	:: sh -> Int           

	-- | The shape of an array of size zero of a particular dimensionality.
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
	--	inRange sh ind == elem ind (range sh)
	inRange	:: sh 	-- ^ Start index for range.
		-> sh 	-- ^ Final index for range.
		-> sh 	-- ^ Index to check for.
		-> Bool

	-- | Compute the intersection of two shapes.
	intersectDim :: sh -> sh -> sh

	-- | What does this do?
	next	:: sh -> sh -> Maybe sh

	-- | Ensure that a shape is completely evaluated.
	infixr 0 `deepSeq`
	deepSeq :: sh -> a -> a

