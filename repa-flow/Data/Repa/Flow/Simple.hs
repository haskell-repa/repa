
module Data.Repa.Flow.Simple
        ( module Data.Repa.Flow.States
        , Source
        , Sink

          -- * Conversions
        , fromList
        , toList
        , takeList

          -- * Flow Operators
          -- ** Constructors
        , repeat_i
        , replicate_i
        , prepend_i

          -- ** Mapping
        , map_i,        map_o

          -- ** Connecting
        , dup_oo,       dup_io,         dup_oi
        , connect_i

          -- ** Splitting
        , head_i
        , peek_i

          -- ** Grouping
        , groups_i

          -- ** Packing
        , pack_ii

          -- ** Folding
        , folds_ii

          -- ** Watching
        , watch_i
        , watch_o
        , trigger_o

          -- ** Ignorance
        , discard_o
        , ignore_o

          -- ** Evaluation
        , drain)

where
import Data.Repa.Flow.States
import Data.Repa.Flow.Simple.Base
import Data.Repa.Flow.Simple.List
import Data.Repa.Flow.Simple.Operator
import qualified Data.Repa.Flow.Generic.Eval    as G


-- | Pull all available values from the source and push them to the sink.
drain   :: Monad m
        => Source m a -> Sink m a -> m ()
drain = G.drain
{-# INLINE [2] drain #-}
