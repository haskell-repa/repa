
-- | Filtering operators on arrays.
module Data.Repa.Array.Internals.Operator.Filter
        ( filter)
where
import Data.Repa.Array.Generic.Index                    as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import System.IO.Unsafe
import Prelude                                          as P hiding (filter)
#include "repa-array.h"


-- | Keep the elements of an array that match the given predicate.
filter  :: (BulkI lSrc a, TargetI lDst a)
        => Name lDst -> (a -> Bool) -> Array lSrc a -> Array lDst a

filter nDst p arr
 = unsafePerformIO
 $ do   
        let !len    =  A.length arr
        !buf        <- unsafeNewBuffer (create nDst len)

        let loop_filter !ixSrc !ixDst
             | ixSrc >= len        
             = return ixDst

             | otherwise
             = do let !x  = arr `index` ixSrc
                  case p x of
                   False        
                    -> do loop_filter (ixSrc + 1) ixDst

                   True
                    -> do unsafeWriteBuffer buf ixDst x
                          loop_filter (ixSrc + 1) (ixDst + 1)

        lenDst  <- loop_filter 0 0

        buf'    <- unsafeSliceBuffer 0 lenDst buf
        unsafeFreezeBuffer buf'
{-# INLINE filter #-}

