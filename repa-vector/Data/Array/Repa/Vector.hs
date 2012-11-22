
module Data.Array.Repa.Vector
        ( Array
        , Vector


        -- * Delayed Representations
        -- ** Bulk Delayed arrays
        , D
        , fromFunction
        , toFunction
        , delay

        -- * Parallel Flows
        , O
        , flow
        , Unflow(..)

        -- * Concrete Representations
        -- ** Unboxed Arrays
        , U
        , fromUnboxed
        , toUnboxed

        -- * Array Operators
        -- ** Bulk
        , Bulk (..)
        , length

        -- ** Maps
        , Map(..)

        -- ** Zips
        , Zip(..)
        , zipWith

        -- ** Unzips
        , Unzip(..)

        -- ** Folds
        , Fold(..)
        , sums
        , counts
        , selects

        -- ** Packs
        , Pack(..)
        , filter
        , packs

        -- ** Generates
        , replicate
        , replicates
        , replicate2
        , replicatesSplit

        -- ** Projections
        , gather
        , gather1

        -- * Flattens
        , flatten2)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Operators.Map
import Data.Array.Repa.Vector.Operators.Zip
import Data.Array.Repa.Vector.Operators.Unzip
import Data.Array.Repa.Vector.Operators.Project
import Data.Array.Repa.Vector.Operators.Pack
import Data.Array.Repa.Vector.Operators.Fold
import Data.Array.Repa.Vector.Operators.Replicate
import Data.Array.Repa.Vector.Operators.Flatten
import Prelude 
        hiding (length, replicate, zipWith, filter)
