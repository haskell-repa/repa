
module Data.Array.Repa.Vector
        ( Vector
        , O

        -- * Flow modes
        , FD, FS

        -- * Flow Distributions
        , BB, BN

        -- * Flow Conversions
        , flow,         unflowP
        , toFlow,       fromFlowP

        -- * Maps
        , Map(..)

        -- * Zips
        , Zip(..)
        , zipWith

        -- * Projections
        , Gather(..)

        -- * Segmented operations
        , replicatesP
        , replicatesSplitP)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Map
import Data.Array.Repa.Vector.Operators.Zip
import Data.Array.Repa.Vector.Operators.Project
import Prelude hiding (zipWith)
