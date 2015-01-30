{-# LANGUAGE UndecidableInstances #-}
module Data.Repa.Array.Linear
        ( L(..)
        , Name  (..)
        , Array (..)
        , linear)
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Bulk
#include "repa-array.h"


-- | A linear layout with the elements indexed by integers.
--
--   * Indexing is not bounds checked. Indexing outside the extent
--     yields the corresponding index.
--
data L  = Linear
        { linearLength  :: Int }


-- | Linear layout.
instance Layout L where
 data Name  L           = L
 type Index L           = Int
 create  L len          = Linear len
 extent  (Linear len)   = len
 toIndex   _ ix         = ix
 fromIndex _ ix         = ix


-- | Linear arrays.
instance Bulk L Int where
 data Array L Int       = LArray Int
 layout (LArray len)    = Linear len
 index  (LArray _)  ix  = ix
 {-# INLINE_ARRAY layout #-}
 {-# INLINE_ARRAY index  #-}


-- | Construct a linear array that produces the corresponding index
--   for every element.
--
--   @> toList $ linear 10
--   [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]@
--
linear :: Int -> Array L Int
linear len      = LArray len
{-# INLINE linear #-}

