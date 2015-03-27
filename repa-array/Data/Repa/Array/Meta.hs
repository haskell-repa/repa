
module Data.Repa.Array.Meta 
        ( -- * Delayed arrays
          D(..)
        , fromFunction
        , toFunction
        , delay 

        , D2(..)
        , delay2

          -- * Dense arrays
        , E (..)
        , vector
        , matrix
        , cube

          -- * Linear spaces
        , L(..)
        , linear

          -- * RowWise spaces
        , RW(..)
        , rowWise

          -- * Tupled arrays
        , T2(..)
        , tup2
        , untup2

          -- * Windowed arrays
        , W(..)
        , Windowable (..)
        , windowed
        , entire)
where
import Data.Repa.Array.Meta.Delayed
import Data.Repa.Array.Meta.Delayed2
import Data.Repa.Array.Meta.Dense
import Data.Repa.Array.Meta.Linear
import Data.Repa.Array.Meta.RowWise
import Data.Repa.Array.Meta.Tuple
import Data.Repa.Array.Meta.Window

