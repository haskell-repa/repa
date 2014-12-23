
module Data.Repa.Flow.Simple
        ( Source (..)
        , Sink   (..)

        -- *
        , repeat_i
        , replicate_i)

{-
        -- * Evaluation
        , drain

        -- * Conversions
        , fromList
        , toList
        , takeList

        , map_i,        map_o
        , dup_oo,       dup_io,         dup_oi
        , connect_i
        , pre_i        
        , head_i
        , peek_i
        , groups_i
        , pack_ii
        , folds_ii

        -- * Watching and Triggering
        , watch_i
        , watch_o
        , trigger_o

        -- * Chunking
        , chunk_i
        , unchunk_i
        , unchunk_o

        -- * Ignorance
        , discard_o
        , ignore_o

        -- * IO
        , fileSourceBytes,   hSourceBytes
        , fileSinkBytes,     hSinkBytes
        , fileSourceRecords, hSourceRecords -}
where
import Data.Repa.Flow.Simple.Base
import Data.Repa.Flow.Simple.Operator

{-
import Data.Repa.Flow.Simple.Eval
import Data.Repa.Flow.Simple.List
import Data.Repa.Flow.Simple.Operator
import Data.Repa.Flow.Simple.Chunk
import Data.Repa.Flow.Simple.IO
-}
