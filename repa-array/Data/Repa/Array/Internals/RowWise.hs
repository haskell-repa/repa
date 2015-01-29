
module Data.Repa.Array.Internals.RowWise
        ( RowWise (..)
        , DIM1, DIM2, DIM3, DIM4, DIM5
        , ix1,  ix2,  ix3,  ix4,  ix5)
where
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Layout
import Data.Repa.Array.Internals.Bulk
import Control.Monad
import GHC.Base                 (quotInt, remInt)
#include "repa-array.h"


-- | The RowWise layout maps higher rank indices to linear ones in a
--   row-major order.
--
--   * The mapping between higher ranked indices and linear indices
--     is not bounds checked.
--
data RowWise sh 
        = RowWise !sh
        deriving (Show, Eq)


instance Shape sh 
      => Shape (RowWise sh) where

        rank (RowWise sh)       
                = rank sh
        {-# INLINE rank #-}

        zeroDim = RowWise zeroDim
        {-# INLINE zeroDim #-}

        unitDim = RowWise unitDim
        {-# INLINE unitDim #-}

        intersectDim (RowWise sh1) (RowWise sh2)
                = RowWise (intersectDim sh1 sh2)
        {-# INLINE intersectDim #-}

        addDim (RowWise sh1) (RowWise sh2)
                = RowWise (addDim sh1 sh2)
        {-# INLINE addDim #-}

        size (RowWise sh)
                = size sh
        {-# INLINE size #-}

        inShapeRange (RowWise sh1) (RowWise sh2) (RowWise sh3)
                = inShapeRange sh1 sh2 sh3
        {-# INLINE inShapeRange #-}

        listOfShape  (RowWise sh)
                = listOfShape sh
        {-# INLINE listOfShape #-}

        shapeOfList  xx
                = liftM RowWise $ shapeOfList xx
        {-# INLINE shapeOfList #-}


instance Layout (RowWise Z) where         
        type Index (RowWise Z)   
                = RowWise Z

        extent _        = RowWise Z
        {-# INLINE extent  #-}

        toIndex _ _     = 0
        {-# INLINE toIndex #-}

        fromIndex _ _   = RowWise Z
        {-# INLINE fromIndex #-}


instance ( Layout  (RowWise sh)
         , Index   (RowWise sh) ~ sh)
       =>  Layout  (RowWise (sh :. Int)) where

        type Index (RowWise (sh :. Int))
                = RowWise   (sh :. Int)

        extent     (RowWise sh)
                = RowWise sh
        {-# INLINE extent  #-}

        toIndex    (RowWise (sh1 :. sh2)) (RowWise (sh1' :. sh2'))
                = toIndex (RowWise sh1) sh1' * sh2 + sh2'
        {-# INLINE toIndex #-}

        fromIndex  (RowWise (ds :. d)) n
               = RowWise (fromIndex (RowWise ds) (n `quotInt` d) :. r)
               -- If we assume that the index is in range, there is no point
               -- in computing the remainder for the highest dimension since
               -- n < d must hold. This saves one remInt per element access
               -- which is quite a big deal.
               where r | rank ds == 0  = n
                       | otherwise     = n `remInt` d
        {-# INLINE fromIndex #-}


instance (Layout (RowWise sh), Index (RowWise sh) ~ RowWise sh)
      => Bulk (RowWise sh) (RowWise sh) where
 data Array (RowWise sh) (RowWise sh)   = RArray sh
 layout (RArray sh)                     = RowWise sh
 index  (RArray _) ix                   = ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


-------------------------------------------------------------------------------
type DIM1       = RowWise SH1
type DIM2       = RowWise SH2
type DIM3       = RowWise SH3
type DIM4       = RowWise SH4
type DIM5       = RowWise SH5


-- | Helper for index construction, which constrains the coordinate to be 
--   an @Int@.
ix1 :: Int -> DIM1
ix1 x         = RowWise (Z :. x)
{-# INLINE ix1 #-}

ix2 :: Int -> Int -> DIM2
ix2 y x       = RowWise (Z :. y :. x)
{-# INLINE ix2 #-}

ix3 :: Int -> Int -> Int -> DIM3
ix3 z y x     = RowWise (Z :. z :. y :. x)
{-# INLINE ix3 #-}

ix4 :: Int -> Int -> Int -> Int -> DIM4
ix4 a z y x   = RowWise (Z :. a :. z :. y :. x)
{-# INLINE ix4 #-}

ix5 :: Int -> Int -> Int -> Int -> Int -> DIM5
ix5 b a z y x = RowWise (Z :. b :. a :. z :. y :. x)
{-# INLINE ix5 #-}

