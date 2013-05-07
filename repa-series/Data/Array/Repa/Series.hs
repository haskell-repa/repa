module Data.Array.Repa.Series
        ( Series (..)
        , eatUnboxed
        , eatUnboxed2

          -- * Stream Operators
        , map
        , fold

          -- * Primitives used by the Repa plugin
        , module Data.Array.Repa.Series.Prim)
where
import Data.Array.Repa.Series.Base
import Data.Array.Repa.Series.Fallback
import Data.Array.Repa.Series.Prim
import Prelude hiding (map)
