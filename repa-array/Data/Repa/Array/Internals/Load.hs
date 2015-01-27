
module Data.Repa.Array.Internals.Load
        ( Load      (..)
        , LoadRange (..))
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Array.Repa.Eval.Gang


-- Load -------------------------------------------------------------------------------------------
-- | Compute all elements defined by a delayed array and write them to a
--   manifest target representation.
--  
--   The instance of this class require that the source array has a delayed
--   representation. If you want to use a pre-existing manifest array as the
--   source then `delay` it first.
class (Bulk l a, Target r a) => Load l r a where

 -- | Fill an entire array sequentially.
 loadS          :: Array l a -> Buffer r a -> IO ()

 -- | Fill an entire array in parallel.
 loadP          :: Gang 
                -> Array l a -> Buffer r a -> IO ()


-- LoadRange --------------------------------------------------------------------------------------
-- | Compute a range of elements defined by a delayed array and write them to a
--   manifest target representation.
class Bulk l a => LoadRange l r a where

 -- | Fill a range of an array sequentially.
 loadRangeS     :: Array l a -> Buffer r a -> Index l -> Index l -> IO ()

 -- | Fill a range of an array in parallel.
 loadRangeP     :: Gang 
                -> Array l a -> Buffer r a -> Index l -> Index l -> IO ()

