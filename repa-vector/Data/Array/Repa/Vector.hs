
module Data.Array.Repa.Vector
        ( Vector
        , O

        -- * Maps
        , Map(..)

        -- * Zips
        , Zip(..)
        , zipWith

        -- * Projections
        , Gather(..)

        -- * Segmented folds
        , Fold(..))
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Flow
import Data.Array.Repa.Vector.Operators.Map
import Data.Array.Repa.Vector.Operators.Zip
import Data.Array.Repa.Vector.Operators.Project
import Data.Array.Repa.Vector.Operators.Fold
import Prelude hiding (zipWith)
