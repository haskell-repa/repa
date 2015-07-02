
module Data.Repa.Array.Internals.Bulk
        ( Bulk (..),    BulkI
        , (!)
        , length
        , first,        last
        , toList
        , toLists
        , toListss)
where
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Layout
import Prelude hiding (length, last)
#include "repa-array.h"



-- Bulk -----------------------------------------------------------------------
-- | Class of array representations that we can read elements from in a 
--   random-access manner. 
class Layout l => Bulk l a where

 -- | An Array supplies an element of type @a@ to each position in the
 --   index space associated with layout @l@.
 data Array l a

 -- | O(1). Get the layout of an array.
 layout      :: Array l a -> l

 -- | O(1). Get an element from an array. 
 --   If the provided index is outside the extent of the array then the
 --   result depends on the layout.
 index       :: Array l a -> Index l -> a


-- | Constraint synonym that requires an integer index space.
type BulkI l a = (Bulk l a, Index l ~ Int)


-- | O(1). Alias for `index`.
(!) :: Bulk l a => Array l a -> Index l -> a
(!) = index
{-# INLINE (!) #-}


-- | O(1). Get the number of elements in an array.
length  :: Bulk  l a 
        => Array l a -> Int
length !arr = size (extent (layout arr))
{-# INLINE_ARRAY length #-}


-- | O(1). Take the first element of an array, if there is one.
first   :: BulkI  l a
        => Array l a -> Maybe a
first !arr
 = if length arr >= 1
        then Just $ index arr 0
        else Nothing
{-# INLINE first #-}


-- O(1). Take the last element of an array, if there is one.
last    :: BulkI  l a
        => Array l a -> Maybe a
last !arr
 = if length arr >= 1
        then Just $ index arr (length arr - 1)
        else Nothing
{-# INLINE last #-}


-- Conversion -----------------------------------------------------------------
-- | Convert an array to a list.
toList  :: Bulk  l a
        => Array l a -> [a]
toList !arr
 = loop_fromList [] 0
 where  !lo     = layout arr
        !len    = length arr
        loop_fromList !acc !ix
         | ix >= len    = reverse acc
         | otherwise    
         = let !x       = arr `index`  (fromIndex lo ix)
           in  loop_fromList (x : acc) (ix + 1)
{-# INLINE_ARRAY toList #-}


-- | Convert a nested array to some lists.
toLists  :: ( Bulk l1 (Array l2 a)
            , Bulk l2 a)
         => Array  l1 (Array l2 a)              -- ^ Source array.
         -> [[a]]                               -- ^ Result list.
toLists arr
 = let  !ll'    =  toList arr
   in   map toList ll'
{-# INLINE_ARRAY toLists #-}


-- | Convert a triply nested array to a triply nested list.
toListss :: ( Bulk l1 (Array l2 (Array l3 a))
            , Bulk l2 (Array l3 a)
            , Bulk l3 a)
         => Array  l1 (Array l2 (Array l3 a))   -- ^ Source array.
         -> [[[a]]]                             -- ^ Result list.
toListss arr
 = let  !ll'    = toLists arr
   in   map (map toList) ll'
{-# INLINE_ARRAY toListss #-}

