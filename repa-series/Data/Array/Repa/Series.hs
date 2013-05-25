module Data.Array.Repa.Series
        ( Series (..)
        , Vector (..)

          -- * Running series expressions.
        , runSeries
        , runSeries2

          -- * Series operators.
        , map
        , fold
        , pack

          -- * Selectors
        , Sel1   (..)
        , mkSel1


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
