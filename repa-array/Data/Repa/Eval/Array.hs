
module Data.Repa.Eval.Array
        ( -- * Array Targets
          Target    (..)

          -- * Array Loading
        , Load      (..)
        , LoadRange (..)

        , computeS)
where
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Load           as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Index                    as A
import System.IO.Unsafe
#include "repa-array.h"


-- | Sequential computation of array elements.
--   The desired result representation is specified by the first argument.
computeS  :: Load lSrc lDst a
          => lDst -> Array lSrc a -> Maybe (Array lDst a)
computeS lDst aSrc
 | (size $ extent lDst) == A.length aSrc
 = unsafePerformIO
 $ do   buf     <- unsafeNewBuffer lDst
        loadS aSrc buf
        aDst    <- unsafeFreezeBuffer buf
        return  $ Just aDst

 | otherwise
 = Nothing
{-# INLINE_ARRAY computeS #-}
