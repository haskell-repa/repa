
module Data.Array.Repa.Vector
        ( module Data.Array.Repa.Vector.Shape
        , module Data.Array.Repa.Vector.Index

        , Array
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
        , fromListUnboxed
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

        -- ** Combines
        , combine2

        -- ** Generates
        , replicate
        , replicates
        , replicate2
        , replicatesSplit

        -- * Appends
        , appends

        -- ** Projections
        , gather
        , gather1

        -- * Flattens
        , flatten2)
where
import Data.Array.Repa.Vector.Shape
import Data.Array.Repa.Vector.Index
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Delayed
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Operators.Map
import Data.Array.Repa.Vector.Operators.Zip
import Data.Array.Repa.Vector.Operators.Append
import Data.Array.Repa.Vector.Operators.Unzip
import Data.Array.Repa.Vector.Operators.Project
import Data.Array.Repa.Vector.Operators.Pack
import Data.Array.Repa.Vector.Operators.Combine
import Data.Array.Repa.Vector.Operators.Fold
import Data.Array.Repa.Vector.Operators.Replicate
import Data.Array.Repa.Vector.Operators.Flatten
import Prelude 
        hiding (length, replicate, zipWith, filter)
