{-# LANGUAGE CPP #-}
module Data.Repa.Array
        ( module Data.Repa.Array.Index

        , Name  (..)                
        , Bulk  (..),   BulkI
        , (!)
        , length

          -- * Index arrays
          -- | Index arrays define an index space but do not contain concrete
          --   element values. Indexing into any point in the array produces
          --   the index at that point. Index arrays are typically used to 
          --   provide an array shape to other array operators.

          -- ** Linear spaces
        , L(..)
        , linear

          -- ** RowWise spaces
        , RW(..)
        , rowWise

          -- * Meta arrays

          -- ** Delayed arrays
        , D(..)
        , fromFunction
        , toFunction
        , delay 

          -- ** Windowed arrays
        , W(..)
        , Windowable (..)
        , windowed
        , entire

          -- ** Tupled arrays
        , T2(..)
        , tup2
        , untup2

          -- * Material arrays
          -- | Material arrays are represented as concrete data in memory
          --   and are defined in "Data.Repa.Array.Material". Indexing into these
          --   arrays is not bounds checked, so you may want to use them in
          --   conjunction with a @C@hecked layout.
        , Material

          -- ** Dense arrays
        , E (..)
        , vector
        , matrix
        , cube

          -- * Conversion
        , fromList,     fromListInto
        , toList


          -- * Computation
        , Load
        , Target
        , computeS,     computeIntoS

          -- * Operators
          -- ** Index space
          -- | Index space transforms view the elements of an array in a different
          --   order, but do not compute new elements. They are all constant time
          --   operations as the location of the required element in the source
          --   array is computed on demand.
        , reverse

          -- ** Mapping
        , map -- , zipWith

          -- ** Searching
        , findIndex

          -- ** Sloshing
          -- | Sloshing operators copy array elements into a different arrangement, 
          --   but do not create new element values.
        , concat
        , concatWith
        , intercalate
        , ConcatDict

          -- ** Grouping
        , groups
        , groupsWith
        , GroupsDict

          -- ** Folding
        , folds
        , foldsWith
        , Folds(..)
        , FoldsDict)
where
import Data.Repa.Array.Linear                           as A
import Data.Repa.Array.Dense                            as A
import Data.Repa.Array.RowWise                          as A
import Data.Repa.Array.Delayed                          as A
import Data.Repa.Array.Window                           as A
import Data.Repa.Array.Tuple                            as A
import Data.Repa.Array.Index
import Data.Repa.Eval.Array                             as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Operator.Concat        as A
import Data.Repa.Array.Internals.Operator.Group         as A
import Data.Repa.Array.Internals.Operator.Fold          as A
import qualified Data.Vector.Fusion.Stream.Monadic      as V
import Prelude  hiding (reverse, length, map, zipWith, concat)
#include "repa-array.h"


-- | Classes supported by all material representations.
--
--   We can index them in a random-access manner, 
--   window them in constant time, 
--   and use them as targets for a computation.
-- 
--   In particular, delayed arrays are not material as we cannot use them
--   as targets for a computation.
--
type Material l a
        = (Bulk l a, Windowable l a, Target l a)


-- | O(1). View the elements of a vector in reverse order.
--
-- @
-- > toList $ reverse $ fromList U [0..10 :: Int]
-- [10,9,8,7,6,5,4,3,2,1,0]
-- @
reverse   :: BulkI  l a
          => Array l a -> Array (D l) a

reverse !arr
 = let  !len    = size (extent $ layout arr)
        get ix  = arr `index` (len - ix - 1)
   in   fromFunction (layout arr) get
{-# INLINE_ARRAY reverse #-}


-- | O(len src) Yield `Just` the index of the first element matching the predicate
--   or `Nothing` if no such element exists.
findIndex :: BulkI l a
          => (a -> Bool) -> Array l a -> Maybe Int

findIndex p !arr
 = loop_findIndex V.SPEC 0
 where  
        !len    = size (extent $ layout arr)

        loop_findIndex !sPEC !ix
         | ix >= len    = Nothing
         | otherwise    
         = let  !x      = arr `index` ix
           in   if p x  then Just ix
                        else loop_findIndex sPEC (ix + 1)
        {-# INLINE_INNER loop_findIndex #-}

{-# INLINE_ARRAY findIndex #-}

