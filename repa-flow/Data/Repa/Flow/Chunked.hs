
module Data.Repa.Flow.Chunked
        ( module Data.Repa.Flow.States

        , Sources, Sinks
        , Flow

          -- * Evaluation
        , drainS

          -- * Conversion
        , fromList
        , fromLists
        , toList1
        , toLists1

          -- * Finalizers
        , finalize_i,   finalize_o

          -- * Flow Operators
          -- ** Mapping
        , map_i,        map_o
        , mapChunks_i,  mapChunks_o
        , smapChunks_i, smapChunks_o

          -- ** Splitting
        , head_i

          -- ** Grouping
        , groupsBy_i,   GroupsDict

          -- ** Folding
        , folds_i,      FoldsDict

          -- ** Watching
        , watch_i,      watch_o
        , trigger_o

          -- ** Ignorance
        , discard_o
        , ignore_o)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.Chunked.Operator
import Data.Repa.Flow.States
import qualified Data.Repa.Flow.Generic         as G
#include "repa-stream.h"


-- | Pull all available values from the sources and push them to the sinks.
drainS   :: (Next i, Monad m)
        => Sources i m r a -> Sinks i m r a -> m ()
drainS = G.drainS
{-# INLINE drainS #-}
