module Data.Array.Repa.Series
        ( -- * Series
          RateNat (..)
        , Series  (..)

          -- * Vectors
        , Vector (..)
        , fromUnboxed
        , toUnboxed

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
import Data.Array.Repa.Series.Series
import Data.Array.Repa.Series.Sel
import Data.Array.Repa.Series.Vector
import Data.Array.Repa.Series.Fallback
import Data.Array.Repa.Series.Prim
import Prelude hiding (map)
