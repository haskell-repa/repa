
module Data.Repa.Eval.Array
        ( -- * Array Targets
          Target    (..)

          -- * Array Loading
        , Load      (..)
        , LoadRange (..)

        , computeS)
where
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Load
import Data.Repa.Array.Internals.Bulk
import System.IO.Unsafe
#include "repa-stream.h"


-- | Sequential computation of array elements.
--   The desired result representation is specified by the first argument.
computeS  :: (Load r1 sh e, Target r2 e t)
          => r2 -> Array r1 sh e -> Array r2 sh e
computeS _ arr1
 = unsafePerformIO
 $ do   mvec2   <- unsafeNewBuffer (size $ extent arr1) 
        loadS arr1 mvec2
        unsafeFreezeBuffer (extent arr1) mvec2
{-# INLINE_ARRAY computeS #-}
