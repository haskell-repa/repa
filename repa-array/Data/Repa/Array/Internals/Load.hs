
module Data.Repa.Array.Internals.Load
        (Load   (..))
where
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Eval.Gang
import Control.Monad.Primitive


-- | Compute all elements defined by a delayed array and write them to a
--   manifest target representation.
--
--   The instances of this class require that the source array has a delayed
--   representation. If you want to use a pre-existing manifest array as the
--   source then `delay` it first.
--
class (Bulk l1 a, Target l2 a) => Load l1 l2 a where

 -- | Fill an entire array sequentially.
 loadS          :: Array l1 a -> IOBuffer l2 a -> IO ()

 -- | Fill an entire array in parallel.
 loadP          :: Gang
                -> Array l1 a -> IOBuffer l2 a -> IO ()

