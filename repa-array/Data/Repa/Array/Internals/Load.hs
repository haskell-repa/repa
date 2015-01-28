
module Data.Repa.Array.Internals.Load
        ( Load      (..)
        , LoadRange (..))
where
import Data.Repa.Array.Index
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Bulk
import Data.Array.Repa.Eval.Gang


-- Load -----------------------------------------------------------------------
-- | Compute all elements defined by a delayed array and write them to a
--   manifest target representation.
--  
--   The instance of this class require that the source array has a delayed
--   representation. If you want to use a pre-existing manifest array as the
--   source then `delay` it first.
class (Bulk lSrc a, Target lDst a) => Load lSrc lDst a where

 -- | Fill an entire array sequentially.
 loadS          :: Array lSrc a -> Buffer lDst a -> IO ()

 -- | Fill an entire array in parallel.
 loadP          :: Gang 
                -> Array lSrc a -> Buffer lDst a -> IO ()


-- LoadRange ------------------------------------------------------------------
-- | Compute a range of elements defined by a delayed array and write them to
--   a manifest target representation.
class Bulk lSrc a => LoadRange lSrc lDst a where

 -- | Fill a range of an array sequentially.
 loadRangeS     :: Array lSrc a 
                -> Buffer lDst a 
                -> Index lSrc -> Index lSrc -> IO ()

 -- | Fill a range of an array in parallel.
 loadRangeP     :: Gang 
                -> Array  lSrc a 
                -> Buffer lDst a 
                -> Index lSrc -> Index lSrc -> IO ()

