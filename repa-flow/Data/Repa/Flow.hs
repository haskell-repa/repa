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
-- > import Data.Repa.Flow
-- > import Data.Repa.Flow.Default.Debug
-- > ws <- fromFiles' [\"\/usr\/share\/dict\/words\", \"\/usr\/share\/dict\/cracklib-small\"] sourceLines
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
-- > up <- map_i B (mapS U toUpper) ws
-- > more 0 up
-- Just [\"UTOPIAN\",\"UTOPIAN'S\",\"UTOPIANS\",\"UTOPIAS\",\"UTRECHT\" ...]
-- @ 
--
--   The `B` and `U` are `Layout` names that indicate how the chunks for the
--   result streams should be arranged in memory. In this case the chunks
--   are `B`-oxed arrays of `U`-nboxed arrays of characters. Other useful
--   layouts are `F` which stores data in foreign memory, and `N` for nested
--   arrays.
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
-- > out <- toFiles ["out1.txt", "out2.txt"] $ sinkLines B U
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
-- > drainS up out
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
        , fromList,             fromLists
        , toList1,              toLists1

        -- * Finalizers
        , finalize_i,           finalize_o

        -- * Flow Operators
        -- ** Mapping
          -- | If you want to work on a chunk at a time then use 
          --   `Data.Repa.Flow.Generic.map_i` and
          --   `Data.Repa.Flow.Generic.map_o` from "Data.Repa.Flow.Generic".
        , map_i,                map_o

        -- ** Connecting
        , dup_oo
        , dup_io
        , dup_oi
        , connect_i

        -- ** Watching
        , watch_i,              watch_o
        , trigger_o

        -- ** Ignorance
        , discard_o
        , ignore_o

        -- ** Splitting
        , head_i

        -- ** Grouping
        , groups_i
        , groupsBy_i
        , GroupsDict

        -- ** Folding
        -- *** Complete
        , foldlS,               foldlAllS

        -- *** Segmented
        , folds_i,              FoldsDict
        , foldGroupsBy_i,       FoldGroupsDict

        -- * Flow I/O
        , defaultChunkSize

        -- ** Buckets
        , module Data.Repa.Flow.IO.Bucket

        -- ** Sourcing
        , sourceCSV
        , sourceTSV
        , sourceRecords
        , sourceLines
        , sourceChars
        , sourceBytes

        -- ** Sinking
        , sinkChars
        , sinkLines
        , sinkBytes
        )
where
import Data.Repa.Flow.Auto
import Data.Repa.Flow.Auto.Debug
import Data.Repa.Flow.Auto.IO
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Flow.States

import Data.Repa.Array.Generic
        hiding (fromList, GroupsDict, FoldsDict)

import Data.Repa.Array.Material
        hiding (fromLists)


