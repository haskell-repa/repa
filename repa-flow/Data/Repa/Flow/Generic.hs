
-- | Everything flows.
module Data.Repa.Flow.Generic
        ( module Data.Repa.Flow.States
        , Sources       (..)
        , Sinks         (..)

        -- * Conversion
        , fromList
        , toList1
        , takeList1

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
        , head_i)
where
import Data.Repa.Flow.States
import Data.Repa.Flow.Generic.Base
import Data.Repa.Flow.Generic.List
import Data.Repa.Flow.Generic.Operator

