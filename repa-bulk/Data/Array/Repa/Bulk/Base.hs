
module Data.Array.Repa.Bulk.Base
        ( Bulk (..)
        , Vector
        , (!)
        , toList
        , toLists
        , toListss)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Index


-- Bulk -------------------------------------------------------------------------------------------
-- | Class of array representations that we can read elements from
--   in a random access manner.
class Shape sh => Bulk r sh e where

 -- | Arrays with a representation tag, shape, and element type.
 --   Use one of the type tags like `D`, `U` and so on for @r@, 
 --   one of `DIM1`, `DIM2` ... for @sh@.
 data Array r sh e

 -- | Take the extent (size) of an array.
 extent         :: Array r sh e -> sh

 -- | Shape polymorphic indexing.
 --
 --   The safety of this indexing operator depends on the array representation.
 index          :: Array r sh e -> sh -> e


-- | O(1). Alias for `index`
(!) :: Bulk r sh a => Array r sh a -> sh -> a
(!) = index


-- | Vectors are 1-dimensional arrays.
type Vector r e = Array r DIM1 e


-- Conversion -------------------------------------------------------------------------------------
-- | Convert an array to a list.
toList 
        ::  Bulk r sh1 a
        => Array r sh1 a
        -> [a]
toList arr
 = loop_fromList [] 0
 where  !sh     = extent arr
        !len    = size sh
        loop_fromList !acc !ix
         | ix >= len    = reverse acc
         | otherwise    
         = loop_fromList (arr `index` (fromIndex sh ix) : acc) 
                         (ix + 1)
{-# INLINE [1] toList #-}


-- | Convert a nested array to some lists.
toLists
        :: ( Bulk r1 sh1 (Array r2 sh2 a)
           , Bulk r2 sh2 a)
        => Array r1 sh1 (Array r2 sh2 a)                -- ^ Source array.
        -> [[a]]                                        -- ^ Result list.

toLists arr
 = let  ll'    =  toList arr
   in   map toList ll'
{-# INLINE [1] toLists #-}


-- | Convert a triply nested array to a triply nested list.
toListss
        :: ( Bulk r1 sh1 (Array r2 sh2 (Array r3 sh3 a))
           , Bulk r2 sh2 (Array r3 sh3 a)
           , Bulk r3 sh3 a)
        => Array r1 sh1 (Array r2 sh2 (Array r3 sh3 a)) -- ^ Source array.
        -> [[[a]]]                                      -- ^ Result list.

toListss arr
 = let  ll'    = toLists arr
   in   map (map toList) ll'
{-# INLINE [1] toListss #-}

