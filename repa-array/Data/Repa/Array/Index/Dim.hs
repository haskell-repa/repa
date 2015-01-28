
module Data.Repa.Array.Index.Dim
        ( -- * Index types
          Z    (..)
        , (:.)  (..)

          -- * Common shapes
        , SH0, SH1, SH2, SH3, SH4, SH5)
where
import Data.Repa.Array.Index.Shape
#include "repa-stream.h"


-- | An index of dimension zero
data Z  = Z
        deriving (Show, Read, Eq, Ord)


-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head
        = !tail :. !head
        deriving (Show, Read, Eq, Ord)


-- Common shapes
type SH0       = Z
type SH1       = SH0 :. Int
type SH2       = SH1 :. Int
type SH3       = SH2 :. Int
type SH4       = SH3 :. Int
type SH5       = SH4 :. Int


-- Shape ----------------------------------------------------------------------
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


