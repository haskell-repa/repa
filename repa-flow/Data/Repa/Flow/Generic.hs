
-- | Everything flows.
module Data.Repa.Flow.Generic
        ( module Data.Repa.Flow.States
        , Sources       (..)
        , Sinks         (..)

        -- * Operators
        , repeat_i
        , replicate_i
        , map_i,        map_o
        , dup_oo,       dup_io,         dup_oi
        , connect_i)
where
import Data.Repa.Flow.States
import Data.Repa.Flow.Generic.Base
import Data.Repa.Flow.Generic.Operator

