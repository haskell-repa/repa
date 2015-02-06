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
-- > import Data.Repa.Flow             as R
-- > import Data.Repa.Flow.IO.Default  as R
-- > import Data.Repa.Flow.Debug       as R
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
--  * NOTE: Althogh @repa-flow@ can be used productively in the ghci REPL, 
--    performance won't be great because you will be running unspecialised,
--    polymorphic code. For best results you should write a complete
--    program and compile it with @ghc -fllvm -O2 Main.hs@
--
module Data.Repa.Flow
        ( -- * Flow types
          Sources, Sinks
        , Flow

        -- * States and Arrays
        , module Data.Repa.Flow.States
        , module Data.Repa.Eval.Array
        , module Data.Repa.Array
        , module Data.Repa.Array.Material

        -- * Evaluation
        , drain

        -- * Conversion
        , fromList,             fromLists
        , toList1,              toLists1

        -- * Finalizers
        , finalize_i,           finalize_o

        -- * Flow Operators
        -- ** Mapping
        , map_i,                map_o
        , mapChunks_i,          mapChunks_o
        , smapChunks_i,         smapChunks_o

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
        , folds_i,              FoldsDict
        , foldGroupsBy_i,       FoldGroupsDict

        -- * Flow I/O
        , defaultChunkSize

        -- ** Sourcing
        , fromFiles
        , sourceTSV
        , sourceRecords
        , sourceLines
        , sourceChars
        , sourceBytes

        -- ** Sinking
        , toFiles
        , sinkChars
        , sinkLines
        , sinkBytes)
where
import Data.Repa.Flow.Default
import Data.Repa.Flow.Default.Debug
import Data.Repa.Flow.Default.IO
import Data.Repa.Flow.States

import Data.Repa.Eval.Array

import Data.Repa.Array                  
        hiding (fromList, Index, GroupsDict, FoldsDict)

import Data.Repa.Array.Material
        hiding (fromLists)


