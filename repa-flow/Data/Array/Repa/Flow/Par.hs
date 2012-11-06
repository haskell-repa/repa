
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

        -- * Mapping
        , map
        , zip
        , zipWith
        , zipLeft
        , zipLeftWith

        -- * Packing
        , packByTag
        , pack
        , filter)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Map
import Data.Array.Repa.Flow.Par.Generate
import Data.Array.Repa.Flow.Par.Pack
import Prelude hiding (map, zip, zipWith, replicate, filter)

