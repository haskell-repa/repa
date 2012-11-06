
module Data.Array.Repa.Flow.Par
        ( Flow   (..)
        , Distro (..)
        , flow
        , Unflow (..)

        -- * Combinators
        , map
        , zip
        , zipWith
        , zipLeft
        , zipLeftWith)
where
import Data.Array.Repa.Flow.Par.Base
import Data.Array.Repa.Flow.Par.Map
import Prelude hiding (map, zip, zipWith)

