
-- | Class of types that can be used as array shapes and indices.
module Data.Repa.Array.Index.Shape
        ( Shape(..)
        , inShape
        , showShape )
where
#include "repa-stream.h"


-- | Class of types that can be used as array shapes and indices.
class Eq sh => Shape sh where

        -- | Get the number of dimensions in a shape.
        rank           :: sh -> Int

        -- | The shape of an array of size zero, with a particular dimensionality.
        zeroDim        :: sh

        -- | The shape of an array with size one, with a particular dimensionality.
        unitDim        :: sh

        -- | Compute the intersection of two shapes.
        intersectDim   :: sh -> sh -> sh

        -- | Add the coordinates of two shapes componentwise
        addDim         :: sh -> sh -> sh

        -- | Get the total number of elements in an array with this shape.
        size           :: sh -> Int

        -- | Given an array shape and shape polymorphic index, yield a linear index
        --   into a flat, row-major representation.
        toIndex        :: sh -> sh -> Int

        -- | Given an array shape and linear index, yield the shape polymorphic index.
        fromIndex      :: sh -> Int -> sh

        -- | Given a starting and ending index, check if some index is with that range.
        inShapeRange   :: sh -> sh -> sh -> Bool

        -- | Convert a shape into its list of dimensions.
        listOfShape    :: sh -> [Int]

        -- | Convert a list of dimensions to a shape
        shapeOfList    :: [Int] -> Maybe sh


-- | Given an array shape and index, check whether the index is in the shape.
inShape ::  Shape sh => sh -> sh -> Bool
inShape sh ix
        = inShapeRange zeroDim sh ix
{-# INLINE_ARRAY inShape #-}


-- | Nicely format a shape as a string
showShape :: Shape sh => sh -> String
showShape = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . listOfShape
{-# NOINLINE showShape #-}
