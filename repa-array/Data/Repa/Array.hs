
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
          -- ** Delayed arrays
        , D
        , fromFunction
        , toFunction
        , delay 
        , computeS

          -- ** Boxed arrays
        , B, boxed
        , fromVectorB, toVectorB

          -- ** Unboxed arrays
        , U, unboxed
        , fromVectorU, toVectorU

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

          -- ** Mapping
        , map, zipWith

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
        , ragspose3
        , concat
        , concat3)
where
import Data.Repa.Eval.Array                     as R
import Data.Repa.Array.Delayed                  as R
import Data.Repa.Array.Window                   as R
import Data.Repa.Array.Unboxed                  as R
import Data.Repa.Array.Boxed                    as R
import Data.Repa.Array.Unsafe.Nested            as R
import Data.Repa.Array.Internals.Shape          as R
import Data.Repa.Array.Internals.Index          as R
import Data.Repa.Array.Internals.Target         as R
import Data.Repa.Array.Internals.Bulk           as R
import System.IO.Unsafe
import qualified Data.Vector.Unboxed            as U
import Prelude hiding (reverse, length, map, zipWith, concat)


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


-- Concat ---------------------------------------------------------------------
-- | O(len result) Concatenate nested vectors.
concat  :: (Bulk r1 DIM1 (Vector r2 a), Bulk r2 DIM1 a, Target r3 a)
        => Vector r1 (Vector r2 a) -> Vector r3 a
concat vs
 | R.length vs == 0
 = R.vfromList []

 | otherwise
 = unsafePerformIO
 $ do   let !lens  = toVectorU $ computeS $ R.map R.length vs
        let !len   = U.sum lens

        buf     <- unsafeNewBuffer len

        let !iLenY = U.length lens

        let loop !iO !iY !row !iX !iLenX
             | iX >= iLenX
             = if iY >= iLenY - 1
                then return ()
                else let iY'    = iY + 1
                         row'   = vs `index` (Z :. iY')
                         iLenX' = R.length row'
                     in  loop iO iY' row' 0 iLenX'

             | otherwise
             = do let x = row `index` (Z :. iX)
                  unsafeWriteBuffer buf iO x
                  loop (iO + 1) iY row (iX + 1) iLenX

        let !row0   = vs `index` (Z :. 0)
        let !iLenX0 = R.length row0
        loop 0 0 row0 0 iLenX0

        unsafeFreezeBuffer (Z :. len) buf
{-# INLINE [2] concat #-}

