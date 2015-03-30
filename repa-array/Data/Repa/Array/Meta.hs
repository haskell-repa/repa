
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
-- === Array fusion
--
-- Array fusion is achieved via the delayed (`D`) layout 
-- and the `computeS` function. For example:
--
-- @
-- > import Data.Repa.Array
-- > computeS U $ A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- @
--
-- Lets look at the result of the first `map`:
--
-- @
-- > :type A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- A.map (* 2) $ fromList U [1 .. 100 :: Int] 
--     :: Array (D U) Int
-- @
--
-- In the type @Array (D U) Int@, the outer `D` indicates that the array
-- is represented as a function that computes each element on demand.
--
-- Applying a second `map` layers another element-producing function on top:
--
-- @ 
-- > :type A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
--     :: Array (D (D U)) Int
-- @
--
-- At runtime, indexing into an array of the above type involves calling
-- the outer @D@-elayed function, which calls the inner @D@-elayed function,
-- which retrieves source data from the inner @U@-nboxed array. Although
-- this works, indexing into a deep stack of delayed arrays can be quite
-- expensive.
--
-- To fully evaluate a delayed array, use the `computeS` function, 
-- which computes each element of the array sequentially. We pass @computeS@
-- the name of the desired result layout, in this case we use `U` to indicate
-- an unboxed array of values:
--
-- @
-- > :type computeS U $ A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
-- computeS U $ A.map (+ 1) $ A.map (* 2) $ fromList U [1 .. 100 :: Int]
--      :: Array U Int
-- @
--
-- At runtime, each element of the result will be computed by first reading
-- the source element, applying @(*2)@ to it, then applying @(+1)@ to it, 
-- then writing to the result array. Array \"fusion\" is achieved by the fact
-- that result of applying @(*2)@ to an element is used directly, without
-- writing it to an intermediate buffer. 
-- 
-- An added bonus is that during compilation, the GHC simplifier will inline
-- the definitions of `map` and `computeS`, then eliminate the intermediate 
-- function calls. In the compiled code all intermediate values will be stored
-- unboxed in registers, without any overhead due to boxing or laziness.
--
-- When used correctly, array fusion allows Repa programs to run as fast as
-- equivalents in C or Fortran. However, without fusion the programs typically
-- run 10-20x slower (so remember apply `computeS` to delayed arrays).
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
        , tail, init

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
       hiding (map, tail, init)

