module Data.Array.Repa.Series
        ( Stream (..)
        , streamUnboxed
        , streamUnboxed2

          -- * Stream Operators
        , fold

          -- * Primitives used by the Repa plugin
        , module Data.Array.Repa.Series.Prim)
where
import Data.Array.Repa.Series.Stream
import Data.Array.Repa.Series.Fallback
import Data.Array.Repa.Series.Prim

