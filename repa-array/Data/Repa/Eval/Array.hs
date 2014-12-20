
module Data.Repa.Eval.Array
        ( -- * Array Targets
          Target    (..)

          -- * Array Loading
        , Load      (..)
        , LoadRange (..)

        , computeS)
where
import Data.Repa.Array.Internals.Target
import Data.Repa.Array.Internals.Load
import Data.Repa.Array.Internals.Bulk
import Data.Repa.Array.Internals.Shape
import System.IO.Unsafe


-- | Sequential computation of array elements.
computeS :: (Load r1 sh e, Target r2 e)
        => Array r1 sh e -> Array r2 sh e
computeS arr1
 = unsafePerformIO
 $ do   mvec2   <- unsafeNewBuffer (size $ extent arr1) 
        loadS arr1 mvec2
        unsafeFreezeBuffer (extent arr1) mvec2
{-# INLINE [2] computeS #-}
