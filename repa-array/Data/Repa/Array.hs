--
-- | NOTE: This is an ALPHA version of Repa 4. The API is not yet complete with
--   respect to Repa 3. Some important functions are still missing, and the 
--   docs may not be up-to-date.
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
        ( Array
        , Elem, Build

        -- * Basics
        , index
        , (!)
        , length

        -- * Conversion
        , fromList
        , fromLists
        , fromListss

        , toList
        , toLists
        , toListss

        -- * Operators

        -- ** Mapping
        , map
        , map2
        , mapElems

        -- ** Folding
        , foldl
        , sum,  prod
        , mean, std
        , correlate
        , folds
        , foldsWith

        -- ** Filtering
        , filter
        , slices
        , trims
        , trimEnds
        , trimStarts

        -- ** Sloshing
        , reverse
        , concat
        , concatWith
        , intercalate
        , unlines
        , concats
        , ragspose3

        -- ** Inserting
        , insert

        -- ** Searching
        , findIndex

        -- ** Merging
        , merge
        , mergeMaybe

        -- ** Compacting
        , compact
        , compactIn

        -- ** Grouping
        , groups
        , groupsWith

        -- ** Splitting
        , segment
        , segmentOn
        , dice
        , diceSep
        )
where
import Data.Repa.Array.Auto
import Prelude 
       hiding (map, length, reverse, filter, concat, unlines, foldl, sum)


