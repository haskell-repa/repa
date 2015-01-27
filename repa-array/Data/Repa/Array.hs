{-# LANGUAGE CPP #-}
module Data.Repa.Array
        ( module Data.Repa.Array.Shape
          -- * Arrays and Vectors
        , Bulk          (..)
        , Vector
        , (!)
        , length

          -- * Representations
        , Repr          (..)

          -- ** Material arrays
          -- | Material arrays are represented as concrete data in memory
          --   and are defined in "Data.Repa.Array.Material.Safe"
          --                   and "Data.Repa.Array.Material.Unsafe".
          --   Import one or the other depending on whether you want to perform 
          --   bounds checks when indexing into them.
        , Material

          -- ** Delayed arrays
        , D(..)
        , fromFunction
        , toFunction
        , delay 
        , computeS

          -- ** Windowed arrays
        , W(..)
        , Window (..)
        , windowed
        , entire

          -- ** Tupled arrays
        , T2(..)
        , tup2
        , untup2

          -- * Conversion
        , vfromList
        , fromList
        
        , toList

          -- * Operators
          -- ** Index space transforms
          -- | Index space transforms view the elements of an array in a different
          --   order, but do not compute new elements. They are all constant time
          --   operations as the location of the required element in the source
          --   array is computed on demand.
        , reverse

          -- ** Mapping
        , map, zipWith

          -- ** Searching
        , findIndex

          -- ** Sloshing
          -- | Sloshing operators copy array elements into a different arrangement, 
          --   but do not create new element values.
        , concat
        , concatWith
        , intercalate

          -- ** Grouping
        , groupsBy
        , GroupsDict

          -- ** Folding
        , folds, Folds(..)
        , FoldsDict)
where
import Data.Repa.Array.Shape
import Data.Repa.Eval.Array                             as A
import Data.Repa.Array.Delayed                          as A
import Data.Repa.Array.Window                           as A
import Data.Repa.Array.Tuple                            as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Operator.Concat        as A
import Data.Repa.Array.Internals.Operator.Group         as A
import Data.Repa.Array.Internals.Operator.Fold          as A
import qualified Data.Vector.Fusion.Stream.Monadic      as V
import Prelude  hiding (reverse, length, map, zipWith, concat)
#include "repa-stream.h"

-- | Classes supported by all material representations.
--
--   We can index them in a random-access manner, 
--   window them in constant time, 
--   and use them as targets for a computation.
-- 
--   In particular, delayed arrays are not material as we cannot use them
--   as targets for a computation.
--
type Material r t sh a
        = (Bulk r sh a, Window r sh a, Target r a t)


-- | O(1). View the elements of a vector in reverse order.
reverse   :: Bulk r DIM1 a
          => Vector r a -> Vector D a

reverse !vec
 = let  !len           = size (extent vec)
        get (Z :. ix)  = vec `index` (Z :. len - ix - 1)
   in   fromFunction (extent vec) get
{-# INLINE_ARRAY reverse #-}


-- | O(len src) Yield `Just` the index of the first element matching the predicate
--   or `Nothing` if no such element exists.
findIndex :: Bulk r DIM1 a
          => (a -> Bool) -> Vector r a -> Maybe Int

findIndex p !vec
 = loop_findIndex V.SPEC 0
 where  
        !len    = size (extent vec)

        loop_findIndex !sPEC !ix
         | ix >= len    = Nothing
         | otherwise    
         = let  !x      = vec `index` (Z :. ix)
           in   if p x  then Just ix
                        else loop_findIndex sPEC (ix + 1)
        {-# INLINE_INNER loop_findIndex #-}

{-# INLINE_ARRAY findIndex #-}

