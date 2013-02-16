
module Data.Array.Repa.Flow.Seq.Operator.Generate
        ( generate_i
        , enumFromN_i
        , replicate_i
        , replicates_bi)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Seq.Source
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import Prelude hiding (replicate)
import GHC.Exts


-------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying a function to each index.
--
--   Like `flow`, but with a different `Report`.
generate_i :: Elt a => Int# -> (Int# -> a) -> Source FD a
generate_i len load
 | (istate, size, _report, get1, get8)  <- flowGuts len load
 = let  here    = "seq.generate_i"
        report' refIx 
         = do   I# ix   <- iread here refIx 0#
                return  $  R.Generate (I# len) (I# ix)
   in   Source istate size report' get1 get8
{-# INLINE [1] generate_i #-}


-------------------------------------------------------------------------------
-- | Yield a vector containing values @x@, @x+1@ etc.
enumFromN_i
        :: Int#         -- ^ Starting value.
        -> Int#         -- ^ Result length.
        -> Source FD Int

enumFromN_i start len
 | (istate, size, _report, get1, get8) <- flowGuts len (\ix -> I# (start +# ix))
 = let  here    = "seq.enumFromN_i"
        report' refIx
         = do   I# ix   <- iread here refIx 0#
                return  $  R.EnumFromN (I# len) (I# ix)
   in   Source istate size report' get1 get8
{-# INLINE [1] enumFromN_i #-}


-------------------------------------------------------------------------------
-- | Produce an flow of the given length with the same value in each position.
replicate_i :: Elt a => Int# -> a -> Source FD a
replicate_i len x
 | (istate, size, _report, get1, get8) <- flowGuts len (\_ -> x)
 = let  here    = "seq.replicate_i"
        report' refIx
         = do   ix      <- uread here refIx 0
                return  $  R.Replicate (I# len) ix
   in   Source istate size report' get1 get8
{-# INLINE [1] replicate_i #-}


-------------------------------------------------------------------------------
-- | Segmented replicate,
--   where we have functions that produce segment lengths and elements from 
--   a bulk vector.
---
--   TODO: only check for flow end when the current segment is finished.
--   TODO: pulling a single element should also hint how many we can pull next time.
--   TODO: could update total count only when we finish a segment.
replicates_bi
        :: Int#                 -- ^ Total length of result.
        -> (Int# -> Int#)       -- ^ SegmentId -> Segment Length.
        -> (Int# -> a)          -- ^ SegmentId -> Value to emit for this segment.
        -> Source FD a

replicates_bi resultLen getSegLen getValue
 = Source istate size report get1 get8
 where
        here    = "seq.replicates_bi"

        !sCount         = 0     -- How many elements we've emitted so far.
        !sSeg           = 1     -- Id of current segment.
        !sSegLen        = 2     -- Length of current segment.
        !sRemain        = 3     -- Elements remaining to emit in this segment.

        istate
         = SourceStateDelayed
         $ do   state        <- unew 4
                uwrite here state sCount  (0  :: Int)
                uwrite here state sSeg    (-1 :: Int)
                uwrite here state sSegLen (0  :: Int)
                uwrite here state sRemain (0  :: Int)
                return state
        {-# INLINE istate #-}


        size state
         = do   !(I# count)     <- uread here state sCount
                !(I# remain)    <- uread here state sRemain
                return  $ Exact ((resultLen -# count) -# remain)
        {-# INLINE size #-}


        report _
         = do   return  $ R.Replicates (I# resultLen)
        {-# NOINLINE report #-}


        get1 !state push1
         = do   !(I# seg)       <- uread here state sSeg
                !(I# remain)    <- uread here state sRemain
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
                {-# INLINE result #-}

                -- Advance to the next segment.
                result_doneSeg seg
                 = do   
                        -- Add the completed segment to the total count.
                        !(I# count)     <- uread here state sCount
                        !(I# segLen)    <- uread here state sSegLen
                        let !count'     = count +# segLen
                        uwrite here state sCount (I# count')

                        -- Check if we've finished the whole flow.
                        if count' >=# resultLen
                         then push1 Done
                         else result_nextSeg (seg +# 1#)
                {-# INLINE result_doneSeg #-}


                -- Find the next segment with a non-zero length.
                result_nextSeg seg 
                 = do   let !segLen = getSegLen seg
                        if segLen ># 0#
                         then do
                                uwrite here state sSeg    (I# seg)
                                uwrite here state sSegLen (I# segLen)
                                result seg segLen

                         else result_nextSeg (seg +# 1#)
                {-# INLINE result_nextSeg #-}
        {-# INLINE get1 #-}
        

        get8 !state push8
         = result
         where  -- If we're far enough from the segment end then emit
                -- a packet of elements, otherwise tell the consumer
                -- they need to pull a single element at a time.
                result
                 = do   !(I# remain)    <- uread here state sRemain
                        if remain >=# 8#
                         then   result_fromSeg remain
                         else   push8 Pull1
                {-# INLINE result #-}

                -- Emit a packet of elements.
                result_fromSeg remain
                 = do
                        uwrite here state sRemain (I# (remain -# 8#))
                        !(I# seg)       <- uread here state sSeg
                        let !x  = getValue seg
                        push8 $ Yield8 x x x x x x x x
                {-# INLINE result_fromSeg #-}
        {-# INLINE get8 #-}

{-# INLINE [1] replicates_bi #-}

