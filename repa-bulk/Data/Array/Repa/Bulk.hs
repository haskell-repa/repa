
module Data.Array.Repa.Bulk
        ( module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index

          -- * Bulk arrays
        , Bulk (..)
        , Vector
        , (!)

          -- * Array construction
        , Target    (..)
        , Load      (..)
        , LoadRange (..)

          -- * Conversion
        , fromList
        , toList
        , toLists
        , toListss


          -- * Concrete Array Representations
          -- ** Delayed representation
        , D
        , fromFunction
        , toFunction
        , delay 

          -- ** Windowed arrays
        , W
        , Window    (..)
        , windowed
        , entire

          -- * Operators
        , reverse
        , findIndex)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Bulk.Load
import Data.Array.Repa.Bulk.Target
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Window
import Prelude hiding (reverse)


-- | View the elements of a vector in reverse order.
reverse   :: Bulk r DIM1 a
          => Vector r a -> Vector D a

reverse !vec
 = let  !len           = size (extent vec)
        get (Z :. ix)  = vec `index` (Z :. len - ix)
   in   fromFunction (extent vec) get
{-# INLINE [2] reverse #-}


-- | O(n) Yield `Just` the index of the first element matching the predicate
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


