
module Data.Repa.Array.RowWise
        ( RW    (..)
        , Name  (..)
        , Array (..)
        , rowWise

        -- | Synonyms for common layouts.
        , DIM1, DIM2, DIM3, DIM4, DIM5

        -- | Helpers that contrain the coordinates to be @Ints@.
        , ix1,  ix2,  ix3,  ix4,  ix5)
where
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Layout
import Data.Repa.Array.Internals.Bulk
import Control.Monad
import GHC.Base                 (quotInt, remInt)
#include "repa-array.h"


-- | A row-wise layout that maps higher rank indices to linear ones in a
--   row-major order.
--
--   Indices are ordered so the inner-most coordinate varies most frequently:
--
--   @> Prelude.map (fromIndex (RowWise (ish2 2 3))) [0..5]
--   [(Z :. 0) :. 0, (Z :. 0) :. 1, (Z :. 0) :. 2, 
--    (Z :. 1) :. 0, (Z :. 1) :. 1, (Z :. 1) :. 2]@
--
--   * Indexing is not bounds checked. Indexing outside the extent 
--     yields the corresponding index.
--
data RW sh 
        = RowWise 
        { rowWiseShape  :: !sh }

deriving instance Eq sh   => Eq   (RW sh)
deriving instance Show sh => Show (RW sh)


-------------------------------------------------------------------------------
instance Shape sh 
      => Shape (RW sh) where

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


-------------------------------------------------------------------------------
instance Layout (RW Z) where         
        data Name  (RW Z)       = RZ
        type Index (RW Z)       = Z
        name                    = RZ
        create RZ Z             = RowWise Z
        extent _                = Z
        toIndex _ _             = 0
        fromIndex _ _           = Z
        {-# INLINE_ARRAY name      #-}
        {-# INLINE_ARRAY create    #-}
        {-# INLINE_ARRAY extent    #-}
        {-# INLINE_ARRAY toIndex   #-}
        {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name (RW Z))
deriving instance Show (Name (RW Z))


-------------------------------------------------------------------------------
instance ( Layout  (RW sh)
         , Index   (RW sh) ~ sh)
       =>  Layout  (RW (sh :. Int)) where

        data Name  (RW (sh :. Int))     = RC (Name (RW sh))
        type Index (RW (sh :. Int))     = sh :. Int

        name = RC name

        create (RC nSh) (sh :. i)
         = let RowWise  iSh     = create nSh sh
           in  RowWise (iSh :. i)

        extent     (RowWise sh) = sh

        toIndex    (RowWise (sh1 :. sh2)) (sh1' :. sh2')
                = toIndex (RowWise sh1) sh1' * sh2 + sh2'

        fromIndex  (RowWise (ds :. d)) n
               = fromIndex (RowWise ds) (n `quotInt` d) :. r
               -- If we assume that the index is in range, there is no point
               -- in computing the remainder for the highest dimension since
               -- n < d must hold. This saves one remInt per element access
               -- which is quite a big deal.
               where r | rank ds == 0  = n
                       | otherwise     = n `remInt` d

        {-# INLINE_ARRAY name      #-}
        {-# INLINE_ARRAY create    #-}
        {-# INLINE_ARRAY toIndex   #-}
        {-# INLINE_ARRAY extent    #-}
        {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name (RW sh)) => Eq   (Name (RW (sh :. Int)))
deriving instance Show (Name (RW sh)) => Show (Name (RW (sh :. Int)))


-------------------------------------------------------------------------------
-- | Row-wise arrays.
instance (Layout (RW sh), Index (RW sh) ~ sh)
      => Bulk (RW sh) sh where
 data Array (RW sh) sh          = RArray sh
 layout (RArray sh)             = RowWise sh
 index  (RArray _) ix           = ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


-- | Construct a rowWise array that produces the corresponding index
--   for every element.
--
--   @> toList $ rowWise (ish2 3 2) 
--   [(Z :. 0) :. 0, (Z :. 0) :. 1,
--    (Z :. 1) :. 0, (Z :. 1) :. 1,
--    (Z :. 2) :. 0, (Z :. 2) :. 1]@
--
rowWise :: sh -> Array (RW sh) sh
rowWise sh = RArray sh
{-# INLINE_ARRAY rowWise #-}


-------------------------------------------------------------------------------
type DIM1       = RW SH1
type DIM2       = RW SH2
type DIM3       = RW SH3
type DIM4       = RW SH4
type DIM5       = RW SH5


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

