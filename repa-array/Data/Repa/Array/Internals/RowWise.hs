
module Data.Repa.Array.Internals.RowWise
        ( RowWise (..))
where
import Data.Repa.Array.Index.Layout
import Data.Repa.Array.Index.Dim
import Data.Repa.Array.Index.Shape
import GHC.Base                 (quotInt, remInt)


-- | The RowWise layout maps higher rank indices to linear ones in a
--   dense, row-major order.
--
--   TODO: add a parameter to RowWise for bounds checking, but do not
--   expose in type. Safe/Unsafe should not pollute visible type. 
--   Bounds errors should be caught when producing linear indices
--   from higher-ranked ones.
--
data RowWise sh 
        = RowWise !sh
        deriving (Show, Eq)


instance Layout (RowWise Z) where         
        type Index (RowWise Z)   
                = Z

        extent _        = Z
        {-# INLINE extent  #-}

        toIndex _ _     = 0
        {-# INLINE toIndex #-}

        fromIndex _ _   = Z
        {-# INLINE fromIndex #-}


instance ( Layout (RowWise sh)
         , Index  (RowWise sh) ~ sh)
       =>  Layout (RowWise (sh :. Int)) where

        type Index (RowWise (sh :. Int))
                = sh :. Int

        extent     (RowWise sh)
                = sh
        {-# INLINE extent  #-}

        toIndex    (RowWise (sh1 :. sh2)) (sh1' :. sh2')
                = toIndex (RowWise sh1) sh1' * sh2 + sh2'
        {-# INLINE toIndex #-}

        fromIndex  (RowWise (ds :. d)) n
               = fromIndex (RowWise ds) (n `quotInt` d) :. r
               -- If we assume that the index is in range, there is no point
               -- in computing the remainder for the highest dimension since
               -- n < d must hold. This saves one remInt per element access which
               -- is quite a big deal.
               where r | rank ds == 0  = n
                       | otherwise     = n `remInt` d
        {-# INLINE fromIndex #-}

