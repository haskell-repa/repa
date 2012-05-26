
module Data.Array.Repa.Eval.Load
        ( Load      (..)
        , LoadRange (..))
where
import Data.Array.Repa.Eval.Target
import Data.Array.Repa.Shape
import Data.Array.Repa.Base

-- Load -----------------------------------------------------------------------
-- | Compute all elements defined by an array and write them to a manifest
--   target representation.
--  
--   Note that instances require that the source array to have a delayed
--   representation such as `D` or `C`. If you want to use a pre-existing
--   manifest array as the source then `delay` it first.
class (Source r1 e, Shape sh) => Load r1 sh e where
 -- | Fill an entire array sequentially.
 loadS          :: Target r2 e => Array r1 sh e -> MVec r2 e -> IO ()

 -- | Fill an entire array in parallel.
 loadP          :: Target r2 e => Array r1 sh e -> MVec r2 e -> IO ()


-- FillRange ------------------------------------------------------------------
-- | Compute a range of elements defined by an array and write them to a fillable
--   representation.
class (Source r1 e, Shape sh) => LoadRange r1 sh e where
 -- | Fill a range of an array sequentially.
 loadRangeS     :: Target r2 e => Array r1 sh e -> MVec r2 e -> sh -> sh -> IO ()

 -- | Fill a range of an array in parallel.
 loadRangeP     :: Target r2 e => Array r1 sh e -> MVec r2 e -> sh -> sh -> IO ()


                        
