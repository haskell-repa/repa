
module Data.Repa.Array.Generic
        ( Name
        , Bulk  (..)
        , BulkI
        , (!)
        , length

          -- * Material arrays
          -- | Material arrays are represented as concrete data in memory
          --   and are defined in "Data.Repa.Array.Material". Indexing into these
          --   arrays is not bounds checked, so you may want to use them in
          --   conjunction with a @C@hecked layout.
        , Material

         -- * Meta arrays
        , module Data.Repa.Array.Meta
        

         -- * Operators
         -- ** Conversion
        , fromList,     fromListInto
        , toList

          -- ** Computation
        , Load
        , Target
        , computeS,     computeIntoS

          -- ** Index space
          -- | Index space transforms view the elements of an array in a different
          --   order, but do not compute new elements. They are all constant time
          --   operations as the location of the required element in the source
          --   array is computed on demand.
        , reverse

          -- ** Mapping
        , mapS, map2S

          -- ** Merging
        , merge
        , mergeMaybe

          -- ** Splitting
        , compact
        , compactIn

          -- ** Filtering
        , filter

          -- ** Inserting
        , insert

          -- ** Searching
        , findIndex

          -- ** Sloshing
          -- | Sloshing operators copy array elements into a different arrangement, 
          --   but do not create new element values.
        , concat
        , concatWith,   unlines
        , intercalate
        , ConcatDict

        , partition
        , partitionBy
        , partitionByIx

          -- ** Grouping
        , groups
        , groupsWith
        , GroupsDict

          -- ** Folding
          -- *** Complete fold
        , foldl, sum, prod, mean, std
        , correlate

          -- *** Segmented fold
        , folds
        , foldsWith
        , Folds(..)
        , FoldsDict)
where

import Data.Repa.Array.Index
import Data.Repa.Array.Meta
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Operator.Concat        as A
import Data.Repa.Array.Internals.Operator.Compact       as A
import Data.Repa.Array.Internals.Operator.Filter        as A
import Data.Repa.Array.Internals.Operator.Fold          as A
import Data.Repa.Array.Internals.Operator.Group         as A
import Data.Repa.Array.Internals.Operator.Merge         as A
import Data.Repa.Array.Internals.Operator.Insert        as A
import Data.Repa.Array.Internals.Operator.Partition     as A
import Data.Repa.Array.Internals.Operator.Reduce        as A
import Data.Repa.Eval.Array                             as A
import qualified Data.Vector.Fusion.Stream.Monadic      as V
import Control.Monad
import Prelude  
       hiding   ( reverse, length, map, zipWith, concat, unlines
                , foldl, sum
                , filter)
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


-- | Like `A.map`, but immediately `computeS` the result.
mapS    :: (Bulk lSrc a, Target lDst b, Index lSrc ~ Index lDst) 
        => Name lDst    -- ^ Name of destination layout.
        -> (a -> b)     -- ^ Worker function.
        -> Array lSrc a -- ^ Source array.
        -> Array lDst b
mapS l f !xs = computeS l $! map f xs
{-# INLINE mapS #-}


-- | Like `A.map2`, but immediately `computeS` the result.
map2S   :: (Bulk   lSrc1 a, Bulk lSrc2 b, Target lDst c
           , Index lSrc1 ~ Index lDst
           , Index lSrc2 ~ Index lDst)
        => Name lDst            -- ^ Name of destination layout.
        -> (a -> b -> c )       -- ^ Worker function.
        -> Array lSrc1 a        -- ^ Source array.
        -> Array lSrc2 b        -- ^ Source array
        -> Maybe (Array lDst  c)
map2S l f xs ys
 = liftM (computeS l) $! map2 f xs ys
{-# INLINE map2S #-}



