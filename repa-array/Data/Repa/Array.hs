--
-- | NOTE: This is an ALPHA version of Repa 4. The API is not yet complete with
--   respect to Repa 3. Some important functions are still missing, and the 
--   docs may not be up-to-date.
-- 
--   A Repa array is a wrapper around an underlying container structure that
--   holds the array elements.
--
--  In the type (`Array` @l@ @a@), the @l@ specifies the `Layout` of data,
--  which includes the type of the underlying container, as well as how 
--  the elements should be arranged in that container. The @a@ specifies 
--  the element type.
--
--  === Material layouts 
--
--  Material layouts hold real data and are defined in "Data.Repa.Array.Material".
--
--  For performance reasons, random access indexing into these layouts
--  is not bounds checked. However, all bulk operators like @map@ and @concat@
--  are guaranteed to be safe.
--
--  * `B`  -- Boxed vectors.
--
--  * `U`  -- Adaptive unboxed vectors.
--
--  * `F`  -- Foreign memory buffers.
--
--  * `N`  -- Nested arrays.
--
--
--  === Delayed layouts
--
--  Delayed layouts represent the elements of an array by a function that
--  computes those elements on demand.
--
--  * `D`  -- Functions from indices to elements.
--
--  === Index-space layouts 
--
--  Index-space produce the corresponding index for each element of the array,
--  rather than real data. They can be used to define an array shape
--  without needing to provide element data.
-- 
--  * `L`   -- Linear spaces.
--
--  * `RW`  -- RowWise spaces.
--
--  === Meta layouts
--
--  Meta layouts combine existing layouts into new ones.
--
--  * `W`  -- Windowed arrays.
--
--  * `E`  -- Dense arrays.
--
--  * `T2` -- Tupled arrays.
--  
-- === Array fusion
--
-- Array fusion is achieved via the delayed (`D`) layout 
-- and the `computeS` function. For example:
--
-- @
-- > import Data.Repa.Array
-- > computeS U $ A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- @
--
-- Lets look at the result of the first `map`:
--
-- @
-- > :type A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- A.map (* 2) $ fromList U [1 .. 100 :: Int] 
--     :: Array (D U) Int
-- @
--
-- In the type @Array (D U) Int@, the outer `D` indicates that the array
-- is represented as a function that computes each element on demand.
--
-- Applying a second `map` layers another element-producing function on top:
--
-- @ 
-- > :type A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
--     :: Array (D (D U)) Int
-- @
--
-- At runtime, indexing into an array of the above type involves calling
-- the outer @D@-elayed function, which calls the inner @D@-elayed function,
-- which retrieves source data from the inner @U@-nboxed array. Although
-- this works, indexing into a deep stack of delayed arrays can be quite
-- expensive.
--
-- To fully evaluate a delayed array, use the `computeS` function, 
-- which computes each element of the array sequentially. We pass @computeS@
-- the name of the desired result layout, in this case we use `U` to indicate
-- an unboxed array of values:
--
-- @
-- > :type computeS U $ A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- computeS U $ A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
--      :: Array U Int
-- @
--
-- At runtime, each element of the result will be computed by first reading
-- the source element, applying @(*2)@ to it, then applying @(+1)@ to it, 
-- then writing to the result array. Array \"fusion\" is achieved by the fact
-- that result of applying @(*2)@ to an element is used directly, without
-- writing it to an intermediate buffer. 
-- 
-- An added bonus is that during compilation, the GHC simplifier will inline
-- the definitions of `map` and `computeS`, then eliminate the intermediate 
-- function calls. In the compiled code all intermediate values will be stored
-- unboxed in registers, without any overhead due to boxing or laziness.
--
-- When used correctly, array fusion allows Repa programs to run as fast as
-- equivalents in C or Fortran. However, without fusion the programs typically
-- run 10-20x slower (so remember apply `computeS` to delayed arrays).
--
-- === How to write fast code
--
-- 1. Add @INLINE@ pragmas to all leaf-functions in your code, expecially ones
--    that compute numeric results. Non-inlined lazy function calls can cost
--    upwards of 50 cycles each, while each numeric operator only costs one
--    (or less). Inlining leaf functions also ensures they are specialised at
--    the appropriate numeric types.
--
-- 2. Add bang patterns to all function arguments, and all fields of your data
--    types. In a high-performance Haskell program, the cost of lazy evaluation
--    can easily dominate the run time if not handled correctly. You don't want
--    to rely on the strictness analyser in numeric code because if it does not
--    return a perfect result then the performance of your program will be
--    awful. This is less of a problem for general Haskell code, and in a different
--    context relying on strictness analysis is fine.
--
-- 3. Compile your program with @ghc -O2 -fllvm -optlo-O3@. The LLVM compiler
--    produces better object code that GHC's internal native code generator.
--
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

        , D2(..)
        , delay2

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
        , map,  map2
        , mapS, map2S

          -- ** Filtering
        , filter

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

          -- *** Segmented fold
        , folds
        , foldsWith
        , Folds(..)
        , FoldsDict)
where
import Data.Repa.Array.Index
import Data.Repa.Array.Linear                           as A
import Data.Repa.Array.Dense                            as A
import Data.Repa.Array.RowWise                          as A
import Data.Repa.Array.Delayed                          as A
import Data.Repa.Array.Delayed2                         as A
import Data.Repa.Array.Window                           as A
import Data.Repa.Array.Tuple                            as A
import Data.Repa.Eval.Array                             as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Operator.Concat        as A
import Data.Repa.Array.Internals.Operator.Group         as A
import Data.Repa.Array.Internals.Operator.Fold          as A
import Data.Repa.Array.Internals.Operator.Partition     as A
import Data.Repa.Array.Internals.Operator.Reduce        as A
import Data.Repa.Array.Internals.Operator.Filter        as A
import qualified Data.Vector.Fusion.Stream.Monadic      as V
import Control.Monad
import  Prelude  
        hiding  ( reverse, length, map, zipWith, concat, unlines
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
mapS l f !xs = computeS l $! A.map f xs
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
 = liftM (computeS l) $! A.map2 f xs ys
{-# INLINE map2S #-}


