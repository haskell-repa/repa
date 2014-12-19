
module Data.Array.Repa.Repr.Unsafe.Nested
        ( UN, U.Unbox
        , Array (..)
        , fromLists
        , fromListss)
where
import Data.Array.Repa.Bulk
import Data.Array.Repa.Index
import Data.Array.Repa.Repr.Delayed
import Prelude                                  as P
import qualified Data.Vector.Unboxed            as U


---------------------------------------------------------------------------------------------------
-- | Nested array represented as a flat array of elements, and a segment
--   descriptor that describes how the elements are partitioned into
--   the sub-arrays. Using this representation for multidimentional arrays
--   is significantly more efficient than using a boxed array of arrays, 
--   as there is no need to allocate the sub-arrays individually in the heap.
--
--   With a nested type like:
--   @Array UN (Array UN (Array UU Int))@, the concrete representation consists
--   of four flat unboxed vectors: two for each of the segment descriptors
--   associated with each level of nesting, and one unboxed vector to hold
--   all the integer elements.
--
data UN

instance Bulk r DIM1 a => Bulk UN DIM1 (Vector r a) where

 data Array UN DIM1 (Vector r a)
        = UNArray 
                 !(U.Vector Int)         -- segment start positions.
                 !(U.Vector Int)         -- segment lengths.
                 !(Array r DIM1 a)       -- data values

 extent (UNArray starts _lengths _elems)
        = Z :. U.length starts
 {-# INLINE [1] extent #-}

 index  (UNArray starts lengths elems) (Z :. ix)
  = slice  (Z :. (starts  `U.unsafeIndex` ix)) 
           (Z :. (lengths `U.unsafeIndex` ix)) 
           elems
 {-# INLINE [1] index #-}

 linearIndex arr ix
  = index arr (Z :. ix)
 {-# INLINE [1] linearIndex #-}

 slice  (Z :. start) (Z :. len) (UNArray starts lengths elems)
  = UNArray (U.unsafeSlice start len starts)
            (U.unsafeSlice start len lengths)
            elems
 {-# INLINE [1] slice #-}


deriving instance Show (Vector r a) => Show (Vector UN (Vector r a))


-- Conversion -------------------------------------------------------------------------------------
-- | O(size src) Convert some lists to a nested array.
fromLists 
        :: Target r a 
        => [[a]] -> Vector UN (Vector r a)
fromLists xss
 = let  xs         = concat xss
        Just elems = fromList      (Z :. length xs) xs
        lengths    = U.fromList    $ map P.length xss
        starts     = U.unsafeInit  $ U.scanl (+) 0 lengths
   in   UNArray starts lengths elems
{-# INLINE [1] fromLists #-}
        

-- | O(size src) Convert a triply nested list to a triply nested array.
fromListss 
        :: Target r a 
        => [[[a]]] -> Vector UN (Vector UN (Vector r a))
fromListss xs
 = let  xs1        = concat xs
        xs2        = concat xs1
        Just elems = fromList (Z :. length xs2) xs2
        
        lengths1   = U.fromList   $ map P.length xs
        starts1    = U.unsafeInit $ U.scanl (+) 0 lengths1

        lengths2   = U.fromList   $ map P.length xs1
        starts2    = U.unsafeInit $ U.scanl (+) 0 lengths2

   in   UNArray starts1 lengths1 (UNArray starts2 lengths2 elems)
{-# INLINE [1] fromListss #-}


