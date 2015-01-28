
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
import System.IO.Unsafe
#include "repa-stream.h"


-- | Sequential computation of array elements.
--   The desired result representation is specified by the first argument.
computeS  :: Load lSrc lDst a
          => lDst -> Array lSrc a -> Array lDst a
computeS lDst arr
 = unsafePerformIO
 $ do   buf     <- unsafeNewBuffer lDst
        loadS arr buf
        unsafeFreezeBuffer buf
{-# INLINE_ARRAY computeS #-}
