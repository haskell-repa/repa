{-# OPTIONS -fno-warn-unused-imports #-}
-- | 
-- 
--  = Getting Started
-- 
--   A flow consists of a bundle of individual streams. Here we create
--   a bundle of two streams, using different files for each. Data will
--   be read in chunks, using the default chunk size of 64kBytes.
--
-- @
-- > import Data.Repa.Array           as A
-- > import Data.Repa.Flow            as F
-- > import Data.Repa.Flow.IO         as F
-- > import Data.Repa.Flow.Auto.Debug as F
--
-- > ws <- fromFiles [\"\/usr\/share\/dict\/words\", \"\/usr\/share\/dict\/cracklib-small\"] sourceLines
-- @
--
--   Show the first few elements of the first chunk of the first file.
--
-- @ 
-- > more 0 ws
-- Just [\"A\",\"A's\",\"AA's\",\"AB's\",\"ABM's\",\"AC's\",\"ACTH's\",\"AI's\" ...]
-- @
--
--   The `more` function is helpful for debugging. It pulls a whole chunk from a
--   source, displays the requested number of elements from the front of it, then
--   discards the rest. In production code you could use `head_i` to split a few
--   elements from a stream while retaining the rest.
--
--   Use `more'` to show more elements at a time. We've already pulled the first chunk,
--   so here are the first 100 elements from the second chunk:
--
-- @
-- > more' 0 100 ws
-- Just [\"Jubal\",\"Judah\",\"Judaic\",\"Judaism\",\"Judaism's\",\"Judaisms\",\"Judas\" ...]
-- @
--
--   Use `moret` to display elements in tabular form. Here are the first few elements of
--   the second stream in the bundle:
--
-- @ 
-- > moret 1 ws
-- "10th"   
-- "1st"    
-- "2nd"    
-- "3rd"    
-- "4th"    
-- "5th"    
-- ...
-- @
--
--   Lets convert the characters to upper-case.
--
-- @
-- > import Data.Char
-- > up <- F.map_i (A.map toUpper) ws
-- > more 0 up
-- Just [\"UTOPIAN\",\"UTOPIAN'S\",\"UTOPIANS\",\"UTOPIAS\",\"UTRECHT\" ...]
-- @ 
--
--   Flows are data-parallel, which means operators like `map_i` apply to all
--   streams in the  bundle. The second stream has been converted to upper-case
--   as well:
--
-- @
-- > more 1 up
-- Just [\"BROWNER\",\"BROWNEST\",\"BROWNIAN\",\"BROWNIE\",\"BROWNIE'S\" ...]
-- @
--
--   Lets write out the data to some files. There are two streams in the bundle,
--   so open a file for each stream:
--
-- @
-- > out <- toFiles ["out1.txt", "out2.txt"] sinkLines
-- @
--
--   Note that the @ws@ and @up@ we used before were bundles of stream 
--  `Sources` whereas @out@ is a bundle of stream `Sinks`. When we used
--   the `map_i` operator before the @_i@ (input) suffix indicates that
--   this is  a transformer of `Sources`. There is a related `map_o`
--   (output) operator for `Sinks`.
-- 
--   Now that we have a bundle of `Sources`, and some matching `Sinks`, 
--   we can `drainS` all of the data from the former into the latter.
--
-- @
-- > F.drainS up out
-- @
--
--   At this point we can run an external shell command to check the output.
--
-- @
-- > :! head out1.txt
-- BEARSKIN'S
-- BEARSKINS
-- BEAST
-- BEAST'S
-- BEASTLIER
-- BEASTLIEST
-- BEASTLINESS
-- BEASTLINESS'S
-- BEASTLY
-- BEASTLY'S
-- @
--
-- = Performance
--
--   Althogh @repa-flow@ can be used productively in the ghci REPL, 
--   performance won't be great because you will be running unspecialised,
--   polymorphic code. For best results you should write a complete
--   program and compile it with @ghc -fllvm -O2 Main.hs@. 
--
module Data.Repa.Flow
        ( -- * Flow types
          Sources
        , Sinks
        , Flow
        , sourcesArity
        , sinksArity

        -- * Evaluation
        , drainS
        , drainP

        -- * Conversion
        -- ** List conversion
        , fromList,             fromLists
        , toList1,              toLists1

        -- ** Array conversion
        , fromArray,            fromArrays
        , toArray1,             toArrays1

        -- * Finalizers
        , finalize_i,           finalize_o

        -- * Flow Operators
        -- ** Replicating
        , replicates_i

        -- ** Mapping
        -- | If you want to work on a chunk at a time then use 
        --   `Data.Repa.Flow.Generic.map_i` and
        --   `Data.Repa.Flow.Generic.map_o` from "Data.Repa.Flow.Generic".
        , map_i,                map_o
        , zipWith_i

        -- | Higher arity zipWith functions.
        , module Data.Repa.Flow.Auto.ZipWith

        -- ** Connecting
        , dup_oo
        , dup_io
        , dup_oi
        , connect_i

        -- ** Watching
        , watch_i,              watch_o
        , trigger_o

        -- ** Ignorance
        , ignore_o
        , abandon_o

        -- ** Splitting
        , head_i

        -- ** Concatenation
        , concat_i

        -- ** Selecting
        , select_i,             select_o
        , discard_i,            discard_o
        , mask_i,               mask_o

        -- ** Grouping
        , groups_i
        , groupsBy_i
        , GroupsDict

        -- ** Folding
        -- *** Complete
        , foldlS,               foldlAllS

        -- *** Segmented
        , folds_i,              FoldsDict
        , foldGroupsBy_i,       FoldGroupsDict)
where
import Data.Repa.Flow.Auto
import Data.Repa.Flow.Auto.Debug
import Data.Repa.Flow.Auto.ZipWith
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Flow.States

import Data.Repa.Array.Generic
        hiding (fromList, GroupsDict, FoldsDict)

import Data.Repa.Array.Material
        hiding (fromLists)


