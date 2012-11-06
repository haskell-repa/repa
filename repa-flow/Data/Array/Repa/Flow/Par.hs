
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

        -- * Combinators
        , map
        , zip
        , zipWith
        , zipLeft
        , zipLeftWith)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Map
import Data.Array.Repa.Flow.Par.Generate
import Prelude hiding (map, zip, zipWith, replicate)

