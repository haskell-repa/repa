
module Data.Array.Repa.Bulk.Base
        ( Bulk (..)
        , (!)
        , toList
        , toLists
        , toListss)
where
import Data.Array.Repa.Shape


-- Bulk -------------------------------------------------------------------------------------------
-- | Class of array representations that we can read elements from
--   in a random access manner.
class Bulk r e where

 -- | Arrays with a representation tag, shape, and element type.
 --   Use one of the type tags like `D`, `U` and so on for @r@, 
 --   one of `DIM1`, `DIM2` ... for @sh@.
 data Array r sh e

 -- | O(1). Take the extent (size) of an array.
 extent         :: Shape sh => Array r sh e -> sh

 -- | O(1). Shape polymorphic indexing.
 --
 --   The safety of this indexing operator depends on the array representation.
 index          :: Shape sh => Array r sh e -> sh -> e
 index arr ix   = arr `linearIndex`       toIndex (extent arr) ix
 {-# INLINE index #-}

 -- | O(1). Linear indexing into underlying, row-major, array representation.
 --
 --   The safety of this indexing operator depends on the array representation.
 linearIndex    :: Shape sh => Array r sh e -> Int -> e

 -- | O(1). Restrict an array to a rectangular subrange, 
 --         given a new starting position and extent.
 slice          :: Shape sh => sh -> sh -> Array r sh e -> Array r sh e

-- | O(1). Alias for `index`
(!) :: Shape sh => Bulk r e => Array r sh e -> sh -> e
(!) = index


-- Conversion -------------------------------------------------------------------------------------
-- | Convert an array to a list.
toList 
        :: (Shape sh1, Bulk r a)
        => Array r sh1 a                                -- ^ Source array.
        -> [a]                                          -- ^ Result list.
toList arr
 = loop_fromList [] 0
 where  !len    = size (extent arr)
        loop_fromList !acc !ix
         | ix >= len    = reverse acc
         | otherwise    = loop_fromList (arr `linearIndex` ix : acc) (ix + 1)
{-# INLINE [1] toList #-}


-- | Convert a nested array to some lists.
toLists
        :: ( Shape sh2, Bulk r1 (Array r2 sh2 a)
           , Shape sh1, Bulk r2 a)
        => Array r1 sh1 (Array r2 sh2 a)                -- ^ Source array.
        -> [[a]]                                        -- ^ Result list.

toLists arr
 = let  ll'    =  toList arr
   in   map toList ll'
{-# INLINE [1] toLists #-}


-- | Convert a triply nested array to a triply nested list.
toListss
        :: ( Shape sh3, Bulk r1 (Array r2 sh2 (Array r3 sh3 a))
           , Shape sh2, Bulk r2 (Array r3 sh3 a)
           , Shape sh1, Bulk r3 a)
        => Array r1 sh1 (Array r2 sh2 (Array r3 sh3 a)) -- ^ Source array.
        -> [[[a]]]                                      -- ^ Result list.

toListss arr
 = let  ll'    = toLists arr
   in   map (map toList) ll'
{-# INLINE [1] toListss #-}

