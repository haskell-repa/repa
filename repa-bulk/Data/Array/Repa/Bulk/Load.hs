
module Data.Array.Repa.Bulk.Load
        ( Load      (..)
        , LoadRange (..))
where
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa.Bulk.Target
import Data.Array.Repa.Bulk.Base


-- Load -------------------------------------------------------------------------------------------
-- | Compute all elements defined by a delayed array and write them to a
--   manifest target representation.
--  
--   The instance of this class require that the source array has a delayed
--   representation. If you want to use a pre-existing manifest array as the
--   source then `delay` it first.
class Bulk r1 sh1 a => Load r1 sh1 a where

 -- | Fill an entire array sequentially.
 loadS          :: Target r2 a
                => Array r1 sh1 a -> Buffer r2 a -> IO ()

 -- | Fill an entire array in parallel.
 loadP          :: Target r2 a
                => Gang 
                -> Array r1 sh1 a -> Buffer r2 a -> IO ()


-- LoadRange --------------------------------------------------------------------------------------
-- | Compute a range of elements defined by a delayed array and write them to a
--   manifest target representation.
class Bulk r1 sh1 a => LoadRange r1 sh1 a where

 -- | Fill a range of an array sequentially.
 loadRangeS     :: Target r2 a
                => Array r1 sh1 a -> Buffer r2 a -> sh1 -> sh1 -> IO ()

 -- | Fill a range of an array in parallel.
 loadRangeP     :: Target r2 a
                => Gang 
                -> Array r1 sh1 a -> Buffer r2 a -> sh1 -> sh1 -> IO ()

