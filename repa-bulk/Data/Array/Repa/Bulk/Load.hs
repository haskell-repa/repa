
module Data.Array.Repa.Bulk.Load
        ( Load      (..)
        , LoadRange (..))
where
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa.Bulk.Target
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Shape


-- Load -------------------------------------------------------------------------------------------
-- | Compute all elements defined by a delayed array and write them to a
--   manifest target representation.
--  
--   The instance of this class require that the source array has a delayed
--   representation. If you want to use a pre-existing manifest array as the
--   source then `delay` it first.
class (Bulk r1 e, Shape sh) => Load r1 sh e where

 -- | Fill an entire array sequentially.
 loadS          :: Target r2 e 
                => Array r1 sh e -> Buffer r2 e -> IO ()

 -- | Fill an entire array in parallel.
 loadP          :: Target r2 e 
                => Gang 
                -> Array r1 sh e -> Buffer r2 e -> IO ()


-- LoadRange --------------------------------------------------------------------------------------
-- | Compute a range of elements defined by a delayed array and write them to a
--   manifest target representation.
class (Bulk r1 e, Shape sh) => LoadRange r1 sh e where

 -- | Fill a range of an array sequentially.
 loadRangeS     :: Target r2 e 
                => Array r1 sh e -> Buffer r2 e -> sh -> sh -> IO ()

 -- | Fill a range of an array in parallel.
 loadRangeP     :: Target r2 e 
                => Gang 
                -> Array r1 sh e -> Buffer r2 e -> sh -> sh -> IO ()

