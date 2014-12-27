
module Data.Repa.Flow.Simple
        ( module Data.Repa.Flow.States
        , Source (..)
        , Sink   (..)

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
        , ignore_o
        , discard_o)

{-
        -- * Evaluation
        , drain

        -- * Chunking
        , chunk_i
        , unchunk_i
        , unchunk_o

        -- * IO
        , fileSourceBytes,   hSourceBytes
        , fileSinkBytes,     hSinkBytes
        , fileSourceRecords, hSourceRecords -}
where
import Data.Repa.Flow.States
import Data.Repa.Flow.Simple.Base
import Data.Repa.Flow.Simple.List
import Data.Repa.Flow.Simple.Operator

{-
import Data.Repa.Flow.Simple.Eval
import Data.Repa.Flow.Simple.Chunk
import Data.Repa.Flow.Simple.IO
-}
