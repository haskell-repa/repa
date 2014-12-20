
-- | Everything flows.
module Data.Repa.Flows
        ( Sources (..)
        , Sinks   (..)
        , isWideSource
        , isWideSink

        -- * Flows operators
        -- ** Mapping
        , maps_i

        -- ** Distribution
        , distributes_o
        , ddistributes_o

        -- ** Discarding
        , discards_o)
where
import Data.Repa.Flows.Internals.Base
import Data.Repa.Flows.Internals.Operator


