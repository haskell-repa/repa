
module Data.Array.Repa.Flow
        ( Source (..)
        , Sink   (..)

        -- * Evaluation
        , drain

        -- * Conversions
        , fromList
        , toList
        , takeList

        -- * Flow operators
        , repeat_i
        , replicate_i
        , map_i,        map_o
        , dup_oo,       dup_io,         dup_oi
        , connect_i
        , pre_i        
        , head_i
        , peek_i
        , groups_i
        , pack_ii
        , folds_ii
        , trigger_o

        -- * Chunking
        , chunk_i
        , unchunk_i
        , unchunk_o)
where
import Data.Array.Repa.Flow.Internals.Base
import Data.Array.Repa.Flow.Internals.Eval
import Data.Array.Repa.Flow.Internals.List
import Data.Array.Repa.Flow.Internals.Operator
import Data.Array.Repa.Flow.Internals.Chunk
