
module Data.Repa.Array.Internals.Operator.Partition
        (partition)
where
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Material.Nested          as A
import Data.Repa.Eval.Elt                       as A
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import System.IO.Unsafe
#include "repa-array.h"


-- | Take a desired number of segments, and array of key value pairs where
--   the key is the segment number. Partition the values into the stated
--   number of segments, discarding values where the key falls outside
--   the given range.
--
partition 
        :: (BulkI lSrc (Int, a), Target lDst a, Index lDst ~ Int, Elt a)
        => Name  lDst                   -- ^ Name of destination layout.
        -> Int                          -- ^ Total number of segments.
        -> Array lSrc (Int, a)          -- ^ Segment numbers and values.
        -> Array N (Array lDst a)       -- ^ Result array

partition nDst iSegs aSrc
 | iSegs <= 0
 = A.fromLists nDst []

 | otherwise
 = unsafePerformIO
 $ do
        -- Length of source array.
        let !len     =  A.length aSrc

        -- Segment start positions and lengths.
        let !vStarts =  U.prescanl (+) 0 $ U.replicate iSegs len 
        !mLens       <- UM.replicate iSegs 0

        -- Elements of result array.
        let !lenDst  =  iSegs * len
        !buf         <- unsafeNewBuffer  (A.create nDst lenDst)

        let loop_partition_init !iDst
             | iDst >= lenDst  = return ()
             | otherwise
             = do unsafeWriteBuffer buf iDst zero
                  loop_partition_init (iDst + 1)

        loop_partition_init 0


        let loop_partition !iSrc
             | iSrc >= len     = return ()
             | otherwise
             = do  let !(k, v) = aSrc `A.index` iSrc

                   if k >= iSegs
                    then loop_partition (iSrc + 1)
                    else do
                        -- Current start length of this segment.
                        let !s  =  U.unsafeIndex vStarts k
                        !o      <- UM.unsafeRead mLens   k

                        -- Write element into the result.
                        unsafeWriteBuffer buf  (s + o) v

                        -- Update segment length.
                        UM.unsafeWrite mLens k (o + 1)

                        loop_partition (iSrc + 1)

        loop_partition 0

        vLens   <- U.unsafeFreeze mLens
        aElems  <- unsafeFreezeBuffer buf

        return  $ NArray vStarts vLens aElems
{-# INLINE_ARRAY partition #-}

