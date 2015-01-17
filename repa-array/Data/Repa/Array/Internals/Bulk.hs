
module Data.Repa.Array.Internals.Bulk
        ( Bulk (..)
        , Repr (..)
        , Vector
        , (!)
        , length
        , toList
        , toLists
        , toListss)
where
import Data.Repa.Array.Shape
import Prelude hiding (length)


-- Bulk -------------------------------------------------------------------------------------------
-- | Class of shape polymorphic array representations that we can read elements
--   from in a random access manner.
class (Repr r, Shape sh) => Bulk r sh e where

 -- | Arrays with a representation tag, shape, and element type.
 --   Use one of the type tags like `D`, `U` and so on for @r@, 
 --   one of `DIM1`, `DIM2` ... for @sh@.
 data Array r sh e

 -- | O(1). Yield the extent of an array.
 --   For a 1-dimensional array this is equivalent to its length.
 extent :: Array r sh e -> sh

 -- | O(1). Get an element from an array. 
 --   The safety of this indexing operator depends on the array representation.
 index  :: Array r sh e -> sh -> e

 -- | O(1). Yield the safe version of an unsafe array.
 safe   :: Array r sh e -> Array (Safe r) sh e

 -- | O(1). Yield the unsafe version of a safe array.
 unsafe :: Array r sh e -> Array (Unsafe r) sh e


-- | Vectors are 1-dimensional arrays.
type Vector r e = Array r DIM1 e


-- | O(1). Alias for `index`.
(!) :: Bulk r sh a => Array r sh a -> sh -> a
(!) = index
{-# INLINE [1] (!) #-}


-- | O(1). Get the length of a vector.
--
--   This is the same as `extent`, but specialised to `Vector`.
length :: Bulk r DIM1 a => Vector r a -> Int
length !arr
 = case extent arr of
        Z :. len        -> len
{-# INLINE [1] length #-}


-- Repr -------------------------------------------------------------------------------------------
-- | Class of array representations.
class Repr r where
 -- | A safe    version of the array representation,
 --   which performs bounds checks when indexing.
 type Safe   r

 -- | An unsafe version of the array representation,
 --   which does not perform bounds checks when indexing.
 -- 
 --   * Indexing out of bounds into an unsafe array representation 
 --     will cause the program to crash or give undefined results.
 type Unsafe r 

 -- | Proxy for an array representation. The representations are singletons, 
 --   so there is only one value of a given type. 
 --   Use with an explicit type signature, like @(repr :: B)@.
 repr    :: r


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

