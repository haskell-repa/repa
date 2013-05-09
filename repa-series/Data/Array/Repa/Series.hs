module Data.Array.Repa.Series
        ( Series (..)
        , Vector (..)

          -- * Running series expressions.
        , runSeries
        , runSeries2

          -- * Series operators.
        , map
        , fold

          -- * Primitives used by the Repa plugin
        , Primitives (..)
        , primitives)
where
import Data.Array.Repa.Series.Series
import Data.Array.Repa.Series.Vector
import Data.Array.Repa.Series.Fallback
import Data.Array.Repa.Series.Prim
import Prelude hiding (map)
