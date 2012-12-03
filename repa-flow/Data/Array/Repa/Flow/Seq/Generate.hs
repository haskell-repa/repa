
module Data.Array.Repa.Flow.Seq.Generate
        ( generate
        , replicate
        , replicatesDirect
        , enumFromN)
where
import Data.Array.Repa.Flow.Seq.Base
import GHC.Exts
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import Prelude hiding (replicate)

here = "Data.Array.Repa.Flow.Seq.Generate"

-------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying a function to each index.
generate :: Int# -> (Int# -> a) -> Flow mode a
generate len f
 = Flow start size report get1 get8
 where  
        start
         = do   refCount <- unew 1
                uwrite here refCount 0 0
                return refCount
        {-# INLINE start #-}


        size refCount
         = do   !(I# ix) <- uread refCount 0
                return  $ Exact (len -# ix)
        {-# INLINE size #-}


        report refCount
         = do   ix      <- uread refCount 0
                return  $ R.Generate (I# len) ix
        {-# NOINLINE report #-}


        get1 refCount push1
          = do  !(I# ix)        <- uread refCount 0
                let !remain     = len -# ix
                if remain ># 0#
                 then do
                        uwrite here refCount 0 (I# (ix +# 1#))
                        push1 $ Yield1  (f ix)
                                        (remain >=# 9#)
                 else   push1 Done
        {-# INLINE get1 #-}


        get8 refCount push8
          = do  !(I# ix) <- uread refCount 0
                let !remain     = len -# ix
                if remain >=# 8#
                 then do
                        uwrite here refCount 0 (I# (ix +# 8#))
                        push8 $ Yield8  (f (ix +# 0#))
                                        (f (ix +# 1#))
                                        (f (ix +# 2#))
                                        (f (ix +# 3#))
                                        (f (ix +# 4#))
                                        (f (ix +# 5#))
                                        (f (ix +# 6#))
                                        (f (ix +# 7#))
                 else   push8 Pull1
        {-# INLINE get8 #-}

{-# INLINE [1] generate #-}


-------------------------------------------------------------------------------
-- | Produce an flow of the given length with the same value in each position.
replicate :: Int# -> a -> Flow mode a
replicate n x
        = generate n (\_ -> x)
{-# INLINE [1] replicate #-}


-------------------------------------------------------------------------------
-- | Segmented replicate,
--   where we have functions that produce segment lengths and elements
--   directly.
---
--   TODO: only check for flow end when the current segment is finished.
--   TODO: pulling a single element should also hint how many we can pull next time.
--   TODO: could update total count only when we finish a segment.
replicatesDirect 
        :: Int#                 -- Total length of result.
        -> (Int# -> Int#)       -- SegmentId -> Segment Length.
        -> (Int# -> a)          -- SegmentId -> Value to emit for this segment.
        -> Flow mode a

replicatesDirect resultLen getSegLen getValue
 = Flow start size report get1 get8
 where
        !sCount         = 0     -- How many elements we've emitted so far.
        !sSeg           = 1     -- Id of current segment.
        !sSegLen        = 2     -- Length of current segment.
        !sRemain        = 3     -- Elements remaining to emit in this segment.

        start 
         = do   state        <- unew 4
                uwrite here state sCount  (0  :: Int)
                uwrite here state sSeg    (-1 :: Int)
                uwrite here state sSegLen (0  :: Int)
                uwrite here state sRemain (0  :: Int)
                return state


        size state
         = do   !(I# count)     <- uread state sCount
                !(I# remain)    <- uread state sRemain
                return  $ Exact ((resultLen -# count) -# remain)


        report _
         = do   return  $ R.Replicates (I# resultLen)
        {-# NOINLINE report #-}


        get1 !state push1
         = do   !(I# seg)       <- uread state sSeg
                !(I# remain)    <- uread state sRemain
                result seg remain
          where 
                result seg remain
                 = do   
                        -- Check if there are any more elements to emit for this segment.
                        if remain ># 0#
                         then do
                                -- Emit a result from this segment.
                                uwrite here state sRemain (I# (remain -# 1#))
                                push1 $ Yield1 (getValue seg) (remain >=# 9#)

                         else result_doneSeg seg

                -- Advance to the next segment.
                result_doneSeg seg
                 = do   
                        -- Add the completed segment to the total count.
                        !(I# count)     <- uread state sCount
                        !(I# segLen)    <- uread state sSegLen
                        let !count'     = count +# segLen
                        uwrite here state sCount (I# count')

                        -- Check if we've finished the whole flow.
                        if count' >=# resultLen
                         then push1 Done
                         else result_nextSeg (seg +# 1#)

                -- Find the next segment with a non-zero length.
                result_nextSeg seg 
                 = do   let !segLen = getSegLen seg
                        if segLen ># 0#
                         then do
                                uwrite here state sSeg    (I# seg)
                                uwrite here state sSegLen (I# segLen)
                                result seg segLen

                         else result_nextSeg (seg +# 1#)
        

        get8 !state push8
         = result
         where  -- If we're far enough from the segment end then emit
                -- a packet of elements, otherwise tell the consumer
                -- they need to pull a single element at a time.
                result
                 = do   !(I# remain)    <- uread state sRemain
                        if remain >=# 8#
                         then   result_fromSeg remain
                         else   push8 Pull1

                -- Emit a packet of elements.
                result_fromSeg remain
                 = do
                        uwrite here state sRemain (I# (remain -# 8#))
                        !(I# seg)       <- uread state sSeg
                        let !x  = getValue seg
                        push8 $ Yield8 x x x x x x x x

{-# INLINE [1] replicatesDirect #-}


-------------------------------------------------------------------------------
-- | Yield a vector containing values @x@, @x+1@ etc.
enumFromN 
        :: Int#         -- ^ Starting value.
        -> Int#         -- ^ Result length.
        -> Flow mode Int

enumFromN first len
 = Flow start size report get1 get8
 where
        start
         = do   refCount <- unew 1
                uwrite here refCount 0 (I# len)

                refAcc   <- unew 1
                uwrite here refAcc   0 (I# first)

                return (refCount, refAcc)


        size (refCount, _)
         = do   !(I# count)     <- uread refCount 0
                return  $ Exact count


        report (refCount, _)
         = do   count           <- uread refCount 0
                return  $ R.EnumFromN (I# len) count
        {-# NOINLINE report #-}


        get1 (refCount, refAcc) push1
         = do   !(I# count)     <- uread refCount 0
                if count ># 0#
                 then do 
                        uwrite here refCount 0 (I# (count -# 1#))

                        acc     <- uread refAcc 0
                        uwrite here refAcc   0 (acc + 1)

                        push1 $ Yield1 acc (count >=# 9#)

                 else   push1 Done

        get8 (refCount, refAcc) push8
         = do   !(I# count)     <- uread refCount 0
                if count >=# 8#
                 then do
                        uwrite here refCount 0 (I# (count -# 8#))

                        acc     <- uread refAcc 0
                        uwrite here refAcc   0 (acc + 8)

                        push8 $ Yield8  (acc + 0) (acc + 1) (acc + 2) (acc + 3)
                                        (acc + 4) (acc + 5) (acc + 6) (acc + 7)
                 else   push8 Pull1

{-# INLINE [1] enumFromN #-}


