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
        , first, last
        , head,  tail, init

        -- * Construction
        , empty
        , singleton
        , generateMaybeS
        , generateEitherS

        -- * Conversion
        , fromList
        , fromLists
        , fromListss

        , toList
        , toLists
        , toListss

        -- * Operators

        -- ** Replicating
        , replicates

        -- ** Mapping
        , map
        , map2
        , mapElems

        -- ** Folding
        , foldl
        , folds
        , foldsWith

        -- *** Special Folds
        , sum,  product
        , mean, std
        , correlate

        -- ** Filtering
        , filter
        , slices
        , trims
        , trimEnds
        , trimStarts

        -- ** Zipping
        , zip
        , unzip

        -- ** Sloshing
        , reverse
        , concat
        , concats
        , concatWith
        , unlines
        , intercalate
        , ragspose3

        -- ** Slicing
        , slice

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

        -- ** Products

        -- *** Generic
        , prod
        , unprod

        -- *** Pattern synonyms
        -- | These patterns patterns work on arrays of arbitrary element type.
        --   
        --   (Haddock 2.16.0 has a bug where the type signatures come out monomorphic)
        --
        , pattern Prod2
        , pattern Prod3
        , pattern Prod4
        , pattern Prod5
        , pattern Prod6
        , pattern Prod7
        , pattern Prod8
        , pattern Prod9
        )
where
import Data.Repa.Array.Auto
import Prelude 
 hiding ( map, length, reverse, filter, concat, unlines, foldl
        , sum, product
        , last
        , head, init, tail
        , zip, unzip)


