
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
        , trigger_o)
where
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Eval
import Data.Array.Repa.Flow.List
import Data.Array.Repa.Flow.Operator
