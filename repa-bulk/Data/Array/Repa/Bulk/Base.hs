
module Data.Array.Repa.Bulk.Base
        ( Bulk (..)
        , (!)
        , listOfArray
        , listOfArrays
        , listOfArrayss)
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
listOfArray 
        :: (Shape sh1, Bulk r a)
        => Array r sh1 a                                -- ^ Source array.
        -> [a]                                          -- ^ Result list.
listOfArray arr
 = loop_listOfArray [] 0
 where  !len    = size (extent arr)
        loop_listOfArray !acc !ix
         | ix >= len    = reverse acc
         | otherwise    = loop_listOfArray (arr `linearIndex` ix : acc) (ix + 1)
{-# INLINE [1] listOfArray #-}


-- | Convert a nested array to some lists.
listOfArrays 
        :: ( Shape sh2, Bulk r1 (Array r2 sh2 a)
           , Shape sh1, Bulk r2 a)
        => Array r1 sh1 (Array r2 sh2 a)                -- ^ Source array.
        -> [[a]]                                        -- ^ Result list.

listOfArrays arr
 = let  arr'    = listOfArray arr
   in   map listOfArray arr'
{-# INLINE [1] listOfArrays #-}

-- | Convert a triply nested array to a triply nested list.
listOfArrayss
        :: ( Shape sh3, Bulk r1 (Array r2 sh2 (Array r3 sh3 a))
           , Shape sh2, Bulk r2 (Array r3 sh3 a)
           , Shape sh1, Bulk r3 a)
        => Array r1 sh1 (Array r2 sh2 (Array r3 sh3 a)) -- ^ Source array.
        -> [[[a]]]                                      -- ^ Result list.

listOfArrayss arr
 = let  arr'    = listOfArrays arr
   in   map (map listOfArray) arr'
{-# INLINE [1] listOfArrayss #-}
