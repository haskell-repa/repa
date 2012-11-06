
module Data.Array.Repa.Flow.Seq.Generate
        ( generate
        , replicate
        , replicatesUnboxed
        , replicatesDirect
        , enumFromN)
where
import Data.Array.Repa.Flow.Seq.Base
import GHC.Exts
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
import Prelude hiding (replicate)

-------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying a function to each index.
generate :: Int# -> (Int# -> a) -> Flow r a
generate len f
 = Flow start size report get1 get8
 where  
        start
         = do   refCount <- UM.unsafeNew 1
                UM.unsafeWrite refCount 0 0
                return refCount
        {-# INLINE start #-}


        size refCount
         = do   !(I# ix) <- UM.unsafeRead refCount 0
                return  $ Exact (len -# ix)
        {-# INLINE size #-}


        report refCount
         = do   ix      <- UM.unsafeRead refCount 0
                return  $ R.Generate (I# len) ix
        {-# NOINLINE report #-}


        get1 refCount push1
          = do  !(I# ix)        <- UM.unsafeRead refCount 0
                let !remain     = len -# ix
                if remain ># 0#
                 then do
                        UM.unsafeWrite refCount 0 (I# (ix +# 1#))
                        push1 $ Yield1  (f ix)
                                        (remain >=# 9#)
                 else   push1 Done
        {-# INLINE get1 #-}


        get8 refCount push8
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                let !remain     = len -# ix
                if remain >=# 8#
                 then do
                        UM.unsafeWrite refCount 0 (I# (ix +# 8#))
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
replicate :: forall a r. Int# -> a -> Flow r a
replicate n x
        = generate n get
        where   get :: Int# -> a
                get _ = x
                {-# INLINE get #-}
{-# INLINE [1] replicate #-}


-------------------------------------------------------------------------------
-- | Segmented replicate,
--   reading segment lengths and elements from unboxed vectors.
replicatesUnboxed
        :: U.Unbox a
        => Int#
        -> U.Vector Int
        -> U.Vector a
        -> Flow r a

replicatesUnboxed resultLen segLen segValue
 = let  getSegLen ix
         = case U.unsafeIndex segLen (I# ix) of
                I# i    -> i

        getSegValue ix
         = U.unsafeIndex segValue (I# ix)

   in   replicatesDirect resultLen getSegLen getSegValue


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
        -> Flow r a

replicatesDirect resultLen getSegLen getValue
 = Flow start size report get1 get8
 where
        !sCount         = 0     -- How many elements we've emitted so far.
        !sSeg           = 1     -- Id of current segment.
        !sSegLen        = 2     -- Length of current segment.
        !sRemain        = 3     -- Elements remaining to emit in this segment.

        start 
         = do   state        <- UM.unsafeNew 4
                UM.unsafeWrite state sCount  (0  :: Int)
                UM.unsafeWrite state sSeg    (-1 :: Int)
                UM.unsafeWrite state sSegLen (0  :: Int)
                UM.unsafeWrite state sRemain (0  :: Int)
                return state


        size state
         = do   !(I# count)     <- UM.unsafeRead state sCount
                !(I# remain)    <- UM.unsafeRead state sRemain
                return  $ Exact ((resultLen -# count) -# remain)


        report _
         = do   return  $ R.Replicates (I# resultLen)
        {-# NOINLINE report #-}


        get1 !state push1
         = do   !(I# seg)       <- UM.unsafeRead state sSeg
                !(I# remain)    <- UM.unsafeRead state sRemain
                result seg remain
          where 
                result seg remain
                 = do   
                        -- Check if there are any more elements to emit for this segment.
                        if remain ># 0#
                         then do
                                -- Emit a result from this segment.
                                UM.unsafeWrite state sRemain (I# (remain -# 1#))
                                push1 $ Yield1 (getValue seg) (remain >=# 9#)

                         else result_doneSeg seg

                -- Advance to the next segment.
                result_doneSeg seg
                 = do   
                        -- Add the completed segment to the total count.
                        !(I# count)     <- UM.unsafeRead state sCount
                        !(I# segLen)    <- UM.unsafeRead state sSegLen
                        let !count'     = count +# segLen
                        UM.unsafeWrite state sCount (I# count')

                        -- Check if we've finished the whole flow.
                        if count' >=# resultLen
                         then push1 Done
                         else result_nextSeg (seg +# 1#)

                -- Find the next segment with a non-zero length.
                result_nextSeg seg 
                 = do   let !segLen = getSegLen seg
                        if segLen ># 0#
                         then do
                                UM.unsafeWrite state sSeg    (I# seg)
                                UM.unsafeWrite state sSegLen (I# segLen)
                                result seg segLen

                         else result_nextSeg (seg +# 1#)
        

        get8 !state push8
         = result
         where  -- If we're far enough from the segment end then emit
                -- a packet of elements, otherwise tell the consumer
                -- they need to pull a single element at a time.
                result
                 = do   !(I# remain)    <- UM.unsafeRead state sRemain
                        if remain >=# 8#
                         then   result_fromSeg remain
                         else   push8 Pull1

                -- Emit a packet of elements.
                result_fromSeg remain
                 = do
                        UM.unsafeWrite state sRemain (I# (remain -# 8#))
                        !(I# seg)       <- UM.unsafeRead state sSeg
                        let !x  = getValue seg
                        push8 $ Yield8 x x x x x x x x


{-# INLINE [1] replicatesDirect #-}


-------------------------------------------------------------------------------
-- | Yield a vector containing values @x@, @x+1@ etc.
enumFromN 
        :: Int#         -- ^ Starting value.
        -> Int#         -- ^ Result length.
        -> Flow r Int

enumFromN first len
 = Flow start size report get1 get8
 where
        start
         = do   refCount <- UM.unsafeNew 1
                UM.unsafeWrite refCount 0 (I# len)

                refAcc   <- UM.unsafeNew 1
                UM.unsafeWrite refAcc   0 (I# first)

                return (refCount, refAcc)


        size (refCount, _)
         = do   !(I# count)     <- UM.unsafeRead refCount 0
                return  $ Exact count


        report (refCount, _)
         = do   count           <- UM.unsafeRead refCount 0
                return  $ R.EnumFromN (I# len) count
        {-# NOINLINE report #-}


        get1 (refCount, refAcc) push1
         = do   !(I# count)     <- UM.unsafeRead refCount 0
                if count ># 0#
                 then do 
                        UM.unsafeWrite refCount 0 (I# (count -# 1#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 1)

                        push1 $ Yield1 acc (count >=# 9#)

                 else   push1 Done

        get8 (refCount, refAcc) push8
         = do   !(I# count)     <- UM.unsafeRead refCount 0
                if count >=# 8#
                 then do
                        UM.unsafeWrite refCount 0 (I# (count -# 8#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 8)

                        push8 $ Yield8  (acc + 0) (acc + 1) (acc + 2) (acc + 3)
                                        (acc + 4) (acc + 5) (acc + 6) (acc + 7)
                 else   push8 Pull1

{-# INLINE [1] enumFromN #-}


