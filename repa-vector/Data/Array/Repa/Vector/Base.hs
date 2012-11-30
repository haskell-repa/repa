
module Data.Array.Repa.Vector.Base
        ( Array
        , Vector
        , theGang
        , Elt(..)
        , module Data.Array.Repa.Vector.Index
        , module Data.Array.Repa.Vector.Shape)
where
import Data.Array.Repa.Bulk.Gang
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Vector.Index
import Data.Array.Repa.Vector.Shape
import Control.Concurrent
import System.IO.Unsafe
import GHC.Exts


-- | Arrays with a representation type, shape and element type.
data family Array r sh e


-- | A Repa vector is just an alias for a 1D array.
type Vector r e 
        = Array r DIM1 e



-- TheGang --------------------------------------------------------------------
-- | This globally shared gang is auto-initialised at startup and shared by all
--   Repa computations.
--
--   In a data parallel setting, it does not help to have multiple gangs
--   running at the same time. This is because a single data parallel
--   computation should already be able to keep all threads busy. If we had
--   multiple gangs running at the same time, then the system as a whole would
--   run slower as the gangs would contend for cache and thrash the scheduler.
--
--   If, due to laziness or otherwise, you try to start multiple parallel
--   Repa computations at the same time, then you will get a warning on stderr
--   at runtime.
--
theGang :: Gang
theGang 
 = unsafePerformIO 
 $ do   (I# caps)    <- getNumCapabilities
        forkGang caps
{-# NOINLINE theGang #-}
