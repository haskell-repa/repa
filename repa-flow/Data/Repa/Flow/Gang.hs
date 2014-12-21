
-- | Everything flows.
module Data.Repa.Flow.Gang
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
import Data.Repa.Flow.Gang.Base
import Data.Repa.Flow.Gang.Operator


