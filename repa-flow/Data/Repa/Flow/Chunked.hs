
module Data.Repa.Flow.Chunked
        ( module Data.Repa.Flow.States
        , Sources, Sinks
        , Flow

          -- * Finalizers
        , finalize_i
        , finalize_o

          -- * Flow Operators
          -- ** Mapping
        , map_i,        map_o
        , mapChunks_i,  mapChunks_o
        , smapChunks_i, smapChunks_o

          -- * Evaluation
        , drain)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.Chunked.Operator
import Data.Repa.Flow.States
import qualified Data.Repa.Flow.Generic as G

-- | Pull all available values from the sources and push them to the sinks.
drain   :: (G.Index i, Monad m)
        => Sources i m r a -> Sinks i m r a -> m ()
drain = G.drain
{-# INLINE [2] drain #-}
