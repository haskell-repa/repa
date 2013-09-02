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

          -- * Selectors
        , Sel1   (..)
        , mkSel1


          -- * Processes
        , Process       (..)
        , with, (%)
        , runProcess
        , runProcess2

          -- * Series combinators
        , map
        , map2
        , pack

          -- * Process constructors
        , reduce

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
import Data.Array.Repa.Series.Process
import Prelude hiding (map)
