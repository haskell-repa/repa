
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
class (Source r1 sh e, Target r2 e) => Fill r1 r2 sh e where
 -- | Fill an entire array sequentially.
 fillS          :: Array r1 sh e -> MVec r2 e -> IO ()

 -- | Fill an entire array in parallel.
 fillP          :: Array r1 sh e -> MVec r2 e -> IO ()


-- FillRange ------------------------------------------------------------------
-- | Compute a range of elements defined by an array and write them to a fillable
--   representation.
class (Source r1 sh e, Target r2 e) => FillRange r1 r2 sh e where
 -- | Fill a range of an array sequentially.
 fillRangeS     :: Array r1 sh e -> MVec r2 e -> sh -> sh -> IO ()

 -- | Fill a range of an array in parallel.
 fillRangeP     :: Array r1 sh e -> MVec r2 e -> sh -> sh -> IO ()


                        
