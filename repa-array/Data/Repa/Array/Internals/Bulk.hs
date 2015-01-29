
module Data.Repa.Array.Internals.Bulk
        ( Bulk (..)
        , (!)
        , length
        , toList
        , toLists
        , toListss)
where
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Layout
import Prelude hiding (length)
#include "repa-array.h"


-- Bulk -----------------------------------------------------------------------
-- | Class of shape polymorphic array representations that we can read elements
--   from in a random access manner.
class Layout l => Bulk l a where

 data Array l a

 -- | O(1). Get the layout of an array.
 layout      :: Array l a -> l

 -- | O(1). Get an element from an array. 
 --   If the provided index is outside the extent of the array then the
 --   result depends on the array representation and layout.
 index       :: Array l a -> Index l -> a


-- | O(1). Alias for `index`.
(!) :: Bulk l a => Array l a -> Index l -> a
(!) = index
{-# INLINE (!) #-}


-- | O(1). Get the number of elements in an array.
length  :: Bulk  l a 
        => Array l a -> Int
length !arr = size (extent (layout arr))
{-# INLINE_ARRAY length #-}


-- Conversion -----------------------------------------------------------------
-- | Convert an array to a list.
toList  :: Bulk  l a
        => Array l a -> [a]
toList arr
 = loop_fromList [] 0
 where  !lo     = layout arr
        !len    = length arr
        loop_fromList !acc !ix
         | ix >= len    = reverse acc
         | otherwise    
         = loop_fromList (arr `index` (fromIndex lo ix) : acc) 
                         (ix + 1)
{-# INLINE_ARRAY toList #-}


-- | Convert a nested array to some lists.
toLists  :: ( Bulk l1 (Array l2 a)
            , Bulk l2 a)
         => Array  l1 (Array l2 a)              -- ^ Source array.
         -> [[a]]                               -- ^ Result list.
toLists arr
 = let  ll'    =  toList arr
   in   map toList ll'
{-# INLINE_ARRAY toLists #-}


-- | Convert a triply nested array to a triply nested list.
toListss :: ( Bulk l1 (Array l2 (Array l3 a))
            , Bulk l2 (Array l3 a)
            , Bulk l3 a)
         => Array  l1 (Array l2 (Array l3 a))   -- ^ Source array.
         -> [[[a]]]                             -- ^ Result list.
toListss arr
 = let  ll'    = toLists arr
   in   map (map toList) ll'
{-# INLINE_ARRAY toListss #-}

