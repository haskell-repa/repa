
module Data.Repa.Array.Generic.Load
        ( Load (..)
        , computeS
        , computeIntoS)
where
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Load           as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Generic.Index            as A
import System.IO.Unsafe
#include "repa-array.h"


-- | Sequential computation of delayed array elements.
--
--   Elements of the source array are computed sequentially and 
--   written to a new array of the specified layout.
--
computeS     :: (Load lSrc lDst a, Index lSrc ~ Index lDst)
             =>  Name lDst -> Array lSrc a -> Array lDst a
computeS !nDst !aSrc
 = let  !lDst      = create nDst (extent $ layout aSrc)
        Just aDst  = computeIntoS lDst aSrc
   in   aDst `seq` aDst
{-# INLINE computeS #-}


-- | Like `computeS` but use the provided desination layout.
--
--   The size of the destination layout must match the size of the source
--   array, else `Nothing`.
--
computeIntoS :: Load lSrc lDst a
             => lDst -> Array lSrc a -> Maybe (Array lDst a)
computeIntoS !lDst !aSrc
 | (A.size $ A.extent lDst) == A.length aSrc
 = unsafePerformIO
 $ do   !buf     <- unsafeNewBuffer lDst
        loadS aSrc buf
        !arr     <- unsafeFreezeBuffer buf
        return  $ Just arr

 | otherwise
 =      Nothing
{-# INLINE_ARRAY computeIntoS #-}


