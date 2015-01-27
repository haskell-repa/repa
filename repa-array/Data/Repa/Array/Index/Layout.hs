
module Data.Repa.Array.Index.Layout
        ( Layout  (..)
        , RowWise (..)
        , Unsafe  (..)
        , Safe    (..))
where
import Data.Repa.Array.Index.Shape
import Data.Repa.Array.Index.Dim
import GHC.Base                 (quotInt, remInt)


-- | A layout represents the mapping from higher ranked indices to
--   linear indices, and termines what happens when an array is 
--   indexed out of bounds.
class Shape (Index lo) => Layout lo where

        type Index lo 

        -- | O(1). Yield the extent of an array.
        --   For a 1-dimensional array this is equivalent to its length.
        extent      :: lo -> Index lo

        -- | Given an array shape and shape polymorphic index, 
        --   yield a linear index into an underlying, flat representation.
        toIndex     :: lo -> Index lo -> Int

        -- | Given an array shape and linear index,
        --   yield the shape polymorphic index.
        fromIndex   :: lo -> Int -> Index lo


-- | Array elements are stored in row-wise manner in a flat vector,
--   and accessed polymorphically.
data RowWise ex sh 
        = RowWise !ex !sh
        deriving (Show, Eq)


-------------------------------------------------------------------------------
data Safe       = Safe

instance Layout (RowWise Safe Z) where          -- TODO: add bounds checks
        type Index (RowWise Safe Z)   
                = Z

        extent _        = Z
        {-# INLINE extent  #-}

        toIndex _ _     = 0
        {-# INLINE toIndex #-}

        fromIndex _ _   = Z
        {-# INLINE fromIndex #-}


instance ( Layout (RowWise  Safe sh)
         , Index  (RowWise  Safe sh) ~ sh)
       =>  Layout (RowWise  Safe (sh :. Int)) where

        type Index (RowWise Safe (sh :. Int))
                = sh :. Int

        extent     (RowWise _ sh)
                = sh
        {-# INLINE extent  #-}

        toIndex    (RowWise un (sh1 :. sh2)) (sh1' :. sh2')
                = toIndex (RowWise un sh1) sh1' * sh2 + sh2'
        {-# INLINE toIndex #-}

        fromIndex  (RowWise un (ds :. d)) n
               = fromIndex (RowWise un ds) (n `quotInt` d) :. r
               -- If we assume that the index is in range, there is no point
               -- in computing the remainder for the highest dimension since
               -- n < d must hold. This saves one remInt per element access which
               -- is quite a big deal.
               where r | rank ds == 0  = n
                       | otherwise     = n `remInt` d
        {-# INLINE fromIndex #-}


-------------------------------------------------------------------------------
data Unsafe     = Unsafe

instance Layout (RowWise Unsafe Z) where
        type Index (RowWise Unsafe Z)   
                = Z

        extent _        = Z
        {-# INLINE extent  #-}

        toIndex _ _     = 0
        {-# INLINE toIndex #-}

        fromIndex _ _   = Z
        {-# INLINE fromIndex #-}


instance ( Layout (RowWise  Unsafe sh)
         , Index  (RowWise  Unsafe sh) ~ sh)
       =>  Layout (RowWise  Unsafe (sh :. Int)) where

        type Index (RowWise Unsafe (sh :. Int))
                = sh :. Int

        extent     (RowWise _ sh)
                = sh
        {-# INLINE extent  #-}

        toIndex    (RowWise un (sh1 :. sh2)) (sh1' :. sh2')
                = toIndex (RowWise un sh1) sh1' * sh2 + sh2'
        {-# INLINE toIndex #-}

        fromIndex  (RowWise un (ds :. d)) n
               = fromIndex (RowWise un ds) (n `quotInt` d) :. r
               -- If we assume that the index is in range, there is no point
               -- in computing the remainder for the highest dimension since
               -- n < d must hold. This saves one remInt per element access which
               -- is quite a big deal.
               where r | rank ds == 0  = n
                       | otherwise     = n `remInt` d
        {-# INLINE fromIndex #-}


