{-# LANGUAGE RankNTypes #-}

-- | Class of types that can be used as array shapes and indices.
module Data.Array.Repa.Shape
	( Shape(..)
        , inShape
        , showShape )
where

-- Shape ----------------------------------------------------------------------
-- | Class of types that can be used as array shapes and indices.
class Eq sh => Shape sh where

	-- | Get the number of dimensions in a shape.
	rank	:: sh -> Int

	-- | The shape of an array of size zero, with a particular dimensionality.
	zeroDim	:: sh

	-- | The shape of an array with size one, with a particular dimensionality.
	unitDim :: sh

	-- | Compute the intersection of two shapes.
	intersectDim :: sh -> sh -> sh

	-- | Add the coordinates of two shapes componentwise
	addDim  :: sh -> sh -> sh

	-- | Get the total number of elements in an array with this shape.
	size	:: sh -> Int

	-- | Check whether this shape is small enough so that its flat
	--	indices an be represented as `Int`. If this returns `False` then your
	--	array is too big. Mostly used for writing QuickCheck tests.
	sizeIsValid :: sh -> Bool


	-- | Convert an index into its equivalent flat, linear, row-major version.
	toIndex :: sh	-- ^ Shape of the array.
		-> sh 	-- ^ Index into the array.
		-> Int

	-- | Inverse of `toIndex`.
	fromIndex
		:: sh 	-- ^ Shape of the array.
		-> Int 	-- ^ Index into linear representation.
		-> sh

	-- | Check whether an index is within a given shape.
	inShapeRange
		:: sh 	-- ^ Start index for range.
		-> sh 	-- ^ Final index for range.
		-> sh 	-- ^ Index to check for.
		-> Bool

	-- | Convert a shape into its list of dimensions.
	listOfShape	:: sh -> [Int]

	-- | Convert a list of dimensions to a shape
	shapeOfList	:: [Int] -> sh

	-- | Ensure that a shape is completely evaluated.
	infixr 0 `deepSeq`
	deepSeq :: sh -> a -> a


-- | Check whether an index is a part of a given shape.
inShape :: forall sh
	.  Shape sh
	=> sh 		-- ^ Shape of the array.
	-> sh		-- ^ Index.
	-> Bool

{-# INLINE inShape #-}
inShape sh ix
	= inShapeRange zeroDim sh ix


-- | Nicely format a shape as a string
showShape :: Shape sh => sh -> String
showShape = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . listOfShape

