
module Data.Array.Repa.Flow.Par
        ( Flow   (..)
        , Distro (..)

        -- * Conversion
        , flow
        , Unflow (..)

        -- * Construction
        , generate
        , replicate
        , replicates
        , replicatesSplit
        , enumFromN
        , appends

        -- * Mapping
        , map
        , zip
        , zipWith
        , zipLeft
        , zipLeftWith

        -- * Projection
        , gather

        -- * Packing
        , packByTag
        , packByFlag
        , filter

        -- * Reduction
        , folds
        , sums)
where
import Data.Array.Repa.Flow.Par.Flow
import Data.Array.Repa.Flow.Par.Operator.Map
import Data.Array.Repa.Flow.Par.Operator.Generate
import Data.Array.Repa.Flow.Par.Operator.Append
import Data.Array.Repa.Flow.Par.Operator.Pack
import Data.Array.Repa.Flow.Par.Operator.Project
import Data.Array.Repa.Flow.Par.Operator.Fold
import Prelude hiding (map, zip, zipWith, replicate, filter)

