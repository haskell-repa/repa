
module Data.Array.Repa.Eval.Fill
        ( Fill      (..)
        , FillRange (..))
where
import Data.Array.Repa.Eval.Target
import Data.Array.Repa.Base


-- Fill -----------------------------------------------------------------------
-- | Compute all elements defined by an array and write them to a fillable
--   representation.
--  
--   Note that instances require that the source array to have a delayed
--   representation such as `D` or `C`. If you want to use a pre-existing
--   manifest array as the source then `delay` it first.
class Source r1 e => Fill r1 sh e where
 -- | Fill an entire array sequentially.
 fillS          :: Target r2 e => Array r1 sh e -> MVec r2 e -> IO ()

 -- | Fill an entire array in parallel.
 fillP          :: Target r2 e => Array r1 sh e -> MVec r2 e -> IO ()


-- FillRange ------------------------------------------------------------------
-- | Compute a range of elements defined by an array and write them to a fillable
--   representation.
class Source r1 e => FillRange r1 sh e where
 -- | Fill a range of an array sequentially.
 fillRangeS     :: Target r2 e => Array r1 sh e -> MVec r2 e -> sh -> sh -> IO ()

 -- | Fill a range of an array in parallel.
 fillRangeP     :: Target r2 e => Array r1 sh e -> MVec r2 e -> sh -> sh -> IO ()


                        
