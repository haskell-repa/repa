
module Data.Array.Repa.Repr.Unsafe.Nested
        ( UN, U.Unbox
        , Array (..))
where
import Data.Array.Repa.Bulk
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Repr.Delayed
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad


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

instance Bulk r a => Bulk UN (Array r DIM1 a) where

 data Array UN DIM1 (Array r DIM1 a)
        = UNArray 
                 !(U.Vector Int)         -- segment start positions.
                 !(U.Vector Int)         -- segment lengths.
                 !(Array r DIM1 a)       -- data values

 extent (UNArray starts _lengths _elems)
        = Z :. U.length starts
 {-# INLINE [1] extent #-}

 index  (UNArray starts lengths elems) (Z :. ix)
  = slice  (Z :. (starts `U.unsafeIndex` ix)) (Z :. (lengths `U.unsafeIndex` ix)) elems
 {-# INLINE [1] index #-}

 slice  (Z :. start) (Z :. len) (UNArray starts lengths elems)
  = UNArray (U.unsafeSlice start len starts)
            (U.unsafeSlice start len lengths)
            elems
 {-# INLINE [1] slice #-}


deriving instance Show (Array r DIM1 a) => Show (Array UN DIM1 (Array r DIM1 a))
