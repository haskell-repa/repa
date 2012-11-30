
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

        -- * Packing
        , packByTag
        , pack
        , filter

        -- * Projection
        , gather

        -- * Reduction
        , folds
        , sums)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Map
import Data.Array.Repa.Flow.Par.Generate
import Data.Array.Repa.Flow.Par.Append
import Data.Array.Repa.Flow.Par.Pack
import Data.Array.Repa.Flow.Par.Project
import Data.Array.Repa.Flow.Par.Fold
import Prelude hiding (map, zip, zipWith, replicate, filter)

