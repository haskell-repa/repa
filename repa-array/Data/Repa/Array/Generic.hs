
-- | Generic array API.
--
--  A Repa array is a wrapper around an underlying container structure that
--  holds the array elements.
--
--  In the type (`Array` @l@ @a@), the @l@ specifies the `Layout` of data,
--  which includes the type of the underlying container, as well as how 
--  the elements should be arranged in that container. The @a@ specifies 
--  the element type.
--
--   The operators provided by this module do not depend on any particular
--   array representation.
--
module Data.Repa.Array.Generic
        ( Name

          -- * Array Access
        , Bulk  (..),   BulkI
        , (!)
        , length
        , first,        last

          -- * Array Computation
        , Load
        , Target,       TargetI
        , computeS,     computeIntoS

         -- * Operators
         -- ** Construction
        , empty
        , singleton
        , generateMaybeS,  mapMaybeS
        , generateEitherS, mapEitherS

         -- ** Conversion
        , fromList,     fromListInto
        , toList
        , convert,      copy

          -- ** Replicating
        , replicates

          -- ** Mapping
        , mapS, map2S

          -- ** Merging
        , merge
        , mergeMaybe

          -- ** Splitting
        , compact
        , compactIn

          -- ** Processing
        , process

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

          -- ** Grouping
        , groups
        , groupsWith
        , GroupsDict

          -- ** Folding
          -- *** Complete fold
        , foldl, sum, product, mean, std
        , correlate

          -- *** Segmented fold
        , folds
        , foldsWith
        , Folds(..)
        , FoldsDict)
where
import Data.Repa.Array.Generic.Load                     as A
import Data.Repa.Array.Generic.Index                    as A
import Data.Repa.Array.Meta                             as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Operator.Concat        as A
import Data.Repa.Array.Internals.Operator.Compact       as A
import Data.Repa.Array.Internals.Operator.Filter        as A
import Data.Repa.Array.Internals.Operator.Fold          as A
import Data.Repa.Array.Internals.Operator.Group         as A
import Data.Repa.Array.Internals.Operator.Insert        as A
import Data.Repa.Array.Internals.Operator.Merge         as A
import Data.Repa.Array.Internals.Operator.Process       as A
import Data.Repa.Array.Internals.Operator.Reduce        as A
import Data.Repa.Array.Internals.Operator.Replicate     as A
import qualified Data.Repa.Array.Generic.Convert        as A
import qualified Data.Vector.Fusion.Stream.Monadic      as V
import Control.Monad
import Prelude  
       hiding   ( reverse, length, map, zipWith, concat, unlines
                , foldl, sum, product, last
                , filter)
#include "repa-array.h"


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


-- | O(1). Constant time conversion of one array representation to another.
convert :: A.Convert l1 a1 l2 a2 
        => Name l2 -> Array l1 a1 -> Array l2 a2
convert _ = A.convert
{-# INLINE convert #-}


-- | O(n). Linear time copy of one array representation to another.
-- 
--   This function must be used instead of `convert` when the bit-wise 
--   layout of the two array representations are different.
--
copy    :: (Bulk l1 a, Target l2 a, Index l1 ~ Index l2)
        => Name l2 -> Array l1 a -> Array l2 a
copy n2 arr  = computeS n2 $! delay arr
{-# INLINE copy #-} 


