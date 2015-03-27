
-- | Meta arrays either generate elements on the fly, 
--   or wrap an inner array to provide an extra features.
-- 
--  === Delayed layouts
--
--  Delayed layouts represent the elements of an array by a function that
--  computes those elements on demand.
--
--  * `D`  -- Functions from indices to elements.
--
--  === Index-space layouts 
--
--  Index-space produce the corresponding index for each element of the array,
--  rather than real data. They can be used to define an array shape
--  without needing to provide element data.
-- 
--  * `L`   -- Linear spaces.
--
--  * `RW`  -- RowWise spaces.
--
--  === Combining layouts
--
--  Combining layouts combine existing layouts into new ones.
--
--  * `W`  -- Windowed arrays.
--
--  * `E`  -- Dense arrays.
--
--  * `T2` -- Tupled arrays.
--  
module Data.Repa.Array.Meta 
        ( -- * Delayed arrays
          D(..)
        , fromFunction
        , toFunction
        , delay
        , map

        , D2(..)
        , delay2
        , map2

          -- * Linear spaces
        , L(..)
        , linear

          -- * RowWise spaces
        , RW(..)
        , rowWise

          -- * Windowed arrays
        , W(..)
        , Windowable (..)
        , windowed
        , entire

          -- * Dense arrays
        , E (..)
        , vector
        , matrix
        , cube

          -- * Tupled arrays
        , T2(..)
        , tup2
        , untup2)
where
import Data.Repa.Array.Meta.Delayed
import Data.Repa.Array.Meta.Delayed2
import Data.Repa.Array.Meta.Dense
import Data.Repa.Array.Meta.Linear
import Data.Repa.Array.Meta.RowWise
import Data.Repa.Array.Meta.Tuple
import Data.Repa.Array.Meta.Window

import Prelude
       hiding (map)

