
-- | Class of types that can be used as array shapes and indices.
module Data.Repa.Array.Internals.Shape
        ( -- * Shapes
          Shape(..)

          -- * Shape operators
        , inShape
        , showShape 

          -- * Polymorphic shapes
        , Z     (..)
        , (:.)  (..)
        ,  SH0,  SH1,  SH2,  SH3,  SH4,  SH5
        , ish0, ish1, ish2, ish3, ish4, ish5)
where
#include "repa-array.h"


-- | Class of types that can be used as array shapes and indices.
class Eq sh => Shape sh where

        -- | Get the number of dimensions in a shape.
        rank           :: sh -> Int

        -- | The shape of an array of size zero, with a particular
        --  dimensionality.
        zeroDim        :: sh

        -- | The shape of an array with size one,
        --   with a particular dimensionality.
        unitDim        :: sh

        -- | Compute the intersection of two shapes.
        intersectDim   :: sh -> sh -> sh

        -- | Add the coordinates of two shapes componentwise
        addDim         :: sh -> sh -> sh

        -- | Get the total number of elements in an array with this shape.
        size           :: sh -> Int

        -- | Given a starting and ending index, check if some index is with
        --  that range.
        inShapeRange   :: sh -> sh -> sh -> Bool

        -- | Convert a shape into its list of dimensions.
        listOfShape    :: sh -> [Int]

        -- | Convert a list of dimensions to a shape
        shapeOfList    :: [Int] -> Maybe sh


-------------------------------------------------------------------------------
-- | Given an array shape and index, check whether the index is in the shape.
inShape ::  Shape sh => sh -> sh -> Bool
inShape sh ix
        = inShapeRange zeroDim sh ix
{-# INLINE_ARRAY inShape #-}


-- | Nicely format a shape as a string
showShape :: Shape sh => sh -> String
showShape = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . listOfShape
{-# NOINLINE showShape #-}


-------------------------------------------------------------------------------
instance Shape Int where
        rank _                  = 1
        zeroDim                 = 0
        unitDim                 = 1
        intersectDim s1 s2      = max s1 s2
        addDim       s1 s2      = s1 + s2
        size s                  = s
        inShapeRange i1 i2 i    = i >= i1 && i <= i2
        listOfShape  i          = [i]
        shapeOfList  [i]        = Just i
        shapeOfList  _          = Nothing
        {-# INLINE rank         #-}
        {-# INLINE zeroDim      #-}
        {-# INLINE unitDim      #-}
        {-# INLINE intersectDim #-}
        {-# INLINE addDim       #-}
        {-# INLINE size         #-}
        {-# INLINE inShapeRange #-}
        {-# INLINE listOfShape  #-}
        {-# INLINE shapeOfList  #-}


-------------------------------------------------------------------------------
-- | An index of dimension zero
data Z  = Z
        deriving (Show, Read, Eq, Ord)


-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head
        = !tail :. !head
        deriving (Show, Read, Eq, Ord)


instance Shape Z where
        rank _                  = 0
        {-# INLINE rank #-}

        zeroDim                 = Z
        {-# INLINE zeroDim #-}

        unitDim                 = Z
        {-# INLINE unitDim #-}

        intersectDim _ _        = Z
        {-# INLINE intersectDim #-}

        addDim _ _              = Z
        {-# INLINE addDim #-}

        size _                  = 1
        {-# INLINE size #-}

        inShapeRange Z Z Z      = True
        {-# INLINE inShapeRange #-}

        listOfShape _           = []
        {-# NOINLINE listOfShape #-}

        shapeOfList []          = Just Z
        shapeOfList _           = Nothing
        {-# NOINLINE shapeOfList #-}



instance Shape sh => Shape (sh :. Int) where
        rank   (sh  :. _)
                = rank sh + 1
        {-# INLINE rank #-}

        zeroDim = zeroDim :. 0
        {-# INLINE zeroDim #-}

        unitDim = unitDim :. 1
        {-# INLINE unitDim #-}

        intersectDim (sh1 :. n1) (sh2 :. n2)
                = (intersectDim sh1 sh2 :. (min n1 n2))
        {-# INLINE intersectDim #-}

        addDim (sh1 :. n1) (sh2 :. n2)
                = addDim sh1 sh2 :. (n1 + n2)
        {-# INLINE addDim #-}

        size  (sh1 :. n)
                = size sh1 * n
        {-# INLINE size #-}

        inShapeRange (zs :. z) (sh1 :. n1) (sh2 :. n2)
                = (n2 >= z) && (n2 < n1) && (inShapeRange zs sh1 sh2)
        {-# INLINE inShapeRange #-}

        listOfShape (sh :. n)
         = n : listOfShape sh
        {-# NOINLINE listOfShape #-}

        shapeOfList xx
         = case xx of
                []      -> Nothing
                x : xs  -> do ss <- shapeOfList xs 
                              return $ ss :. x
        {-# NOINLINE shapeOfList #-}


-------------------------------------------------------------------------------
-- Common shapes
type SH0       = Z
type SH1       = SH0 :. Int
type SH2       = SH1 :. Int
type SH3       = SH2 :. Int
type SH4       = SH3 :. Int
type SH5       = SH4 :. Int


ish0 :: SH0
ish0     = Z

ish1 :: Int -> SH1
ish1 x1          = Z :. x1

ish2 :: Int -> Int -> SH2
ish2 x2 x1       = Z :. x2 :. x1

ish3 :: Int -> Int -> Int -> SH3
ish3 x3 x2 x1    = Z :. x3 :. x2 :. x1

ish4 :: Int -> Int -> Int -> Int -> SH4
ish4 x4 x3 x2 x1 = Z :. x4 :. x3 :. x2 :. x1


ish5 :: Int -> Int -> Int -> Int -> Int -> SH5
ish5 x5 x4 x3 x2 x1 = Z :. x5 :. x4 :. x3 :. x2 :. x1

