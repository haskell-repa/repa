
module Data.Repa.Array
        ( -- * Arrays and Vectors
          Bulk      (..)
        , Vector
        , (!)
        , length

          -- * Shapes and Indices
        , Shape     (..)
        , Z (..), (:.) (..)
        , DIM0, DIM1, DIM2, DIM3, DIM4, DIM5
        ,       ix1,  ix2,  ix3,  ix4,  ix5

          -- * Array Representations
          -- ** Unboxed arrays
        , U
        , fromListU,    vfromListU
        , fromVectorU
        , toVectorU

          -- ** Delayed arrays
        , D
        , fromFunction
        , toFunction
        , delay 

          -- ** Windowed arrays
        , W
        , Window    (..)
        , windowed
        , entire

          -- ** Nested arrays
        , UN

          -- * Generic Conversion
        , vfromList
        , fromList
        , fromLists
        , fromListss
        
        , toList
        , toLists
        , toListss

          -- * Array Operators
          -- ** Index space transforms
          -- | Index space transforms view the elements of an array in a different
          --   order, but do not compute new elements. They are all constant time
          --   operations as the location of the required element in the source
          --   array is computed on demand.
        , reverse

          -- ** Splitting
          -- | Splitting operators compute a segment descriptor which describes
          --   how the elements in the source should be arranged into sub-arrays.
          --   The elements of the source array are not copied.
        , segment,      segmentOn
        , dice,         diceOn

          -- ** Searching
        , findIndex

          -- ** Sloshing
          -- | Sloshing operators copy array elements into a different arrangement, 
          --   but do not create new element values.
        , ragspose3)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Unboxed
import Data.Repa.Array.Unsafe.Nested
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Index
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Prelude hiding (reverse, length)


-- | O(1). View the elements of a vector in reverse order.
reverse   :: Bulk r DIM1 a
          => Vector r a -> Vector D a

reverse !vec
 = let  !len           = size (extent vec)
        get (Z :. ix)  = vec `index` (Z :. len - ix - 1)
   in   fromFunction (extent vec) get
{-# INLINE [2] reverse #-}


-- | O(len src) Yield `Just` the index of the first element matching the predicate
--   or `Nothing` if no such element exists.
findIndex :: Bulk r DIM1 a
          => (a -> Bool) -> Vector r a -> Maybe Int

findIndex p !vec
 = loop_findIndex 0
 where  
        !len    = size (extent vec)

        loop_findIndex !ix
         | ix >= len    = Nothing
         | otherwise    
         = let  !x      = vec `index` (Z :. ix)
           in   if p x  then Just ix
                        else loop_findIndex (ix + 1)
        {-# INLINE [0] loop_findIndex #-}

{-# INLINE [2] findIndex #-}


