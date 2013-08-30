module Data.Array.Repa.Series
        ( -- * Rates
          RateNat (..)
        , Down4   (..)
        , Tail4   (..)

          -- * Series
        , Series  (..)

          -- * Vectors
        , Vector (..)
        , fromPrimitive
        , toPrimitive

          -- * Running series expressions
        , runSeries
        , runSeries2
        , runSeries3
        , runSeries4

          -- * Selectors
        , Sel1   (..)
        , mkSel1

          -- * Series operators
        , map
        , map2
        , reduce
        , fold
        , foldIndex
        , pack

          -- * Primitives used by the Repa plugin
        , Primitives (..)
        , primitives)
where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Series
import Data.Array.Repa.Series.Sel
import Data.Array.Repa.Series.Vector
import Data.Array.Repa.Series.Fallback
import Data.Array.Repa.Series.Prim
import Prelude hiding (map)
