{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
module Data.Repa.Array.Material.Auto.Base
        ( A             (..)
        , Name          (..)
        , Array         (..))
where
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Material.Unboxed         as A
import Data.Repa.Array.Material.Foreign         as A
import Data.Repa.Array.Material.Nested          as A
#include "repa-array.h"


-- | Arrays where the elements that are automatically layed out into some
--   efficient runtime representation.
--
--   The implementation uses type families to chose unboxed representations
--   for all elements that can be unboxed. In particular: arrays of unboxed
--   tuples are represented as tuples of unboxed arrays, and nested arrays
--   are represented using a segment descriptor and a single single flat
--   vector containing all the elements.
--
data A  = Auto { autoLength :: Int }
        deriving (Show, Eq)


instance Layout A where
 data Name  A                   = A
 type Index A                   = Int
 name                           = A
 create A len                   = Auto len
 extent (Auto len)              = len
 toIndex   _ ix                 = ix
 fromIndex _ ix                 = ix
 {-# INLINE_ARRAY name      #-}
 {-# INLINE_ARRAY create    #-}
 {-# INLINE_ARRAY extent    #-}
 {-# INLINE_ARRAY toIndex   #-}
 {-# INLINE_ARRAY fromIndex #-}

deriving instance Eq   (Name A)
deriving instance Show (Name A)

