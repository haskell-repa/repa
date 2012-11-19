
module Data.Array.Repa.Vector
        ( Vector
        , O

        -- * Flow modes
        , FD, FS

        -- * Flow Distributions
        , BB, BN

        -- * Flow Conversions
        , flow
        , unflow

        -- * Maps
        , Map(..)

        -- * Zips
        , Zip(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Map
