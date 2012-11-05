
module Data.Array.Repa.Flow.Generate
        ( generate
        , replicate
        , replicatesUnboxed
        , replicatesDirect
        , enumFromN)
where
import Data.Array.Repa.Flow.Base
import GHC.Exts
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Prelude hiding (replicate)

------------------------------------------------------------------------------
-- | Construct a flow of the given length by applying the function to each
--   index.
generate :: Int -> (Int -> a) -> IO (Flow a)
generate (I# len) f
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 0

        let
         getSize
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                return  $ Exact (len -# ix)

         get1 push1
          = do  !(I# ix)        <- UM.unsafeRead refCount 0
                let !remain     = len -# ix
                if remain ># 0#
                 then do
                        UM.unsafeWrite refCount 0 (I# (ix +# 1#))
                        push1 $ Yield1  (f (I# ix))
                                        (remain >=# 9#)
                 else   push1 Done

         get8 push8
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                let !remain     = len -# ix
                if remain >=# 8#
                 then do
                        UM.unsafeWrite refCount 0 (I# (ix +# 8#))
                        push8 $ Yield8  (f (I# (ix +# 0#)))
                                        (f (I# (ix +# 1#)))
                                        (f (I# (ix +# 2#)))
                                        (f (I# (ix +# 3#)))
                                        (f (I# (ix +# 4#)))
                                        (f (I# (ix +# 5#)))
                                        (f (I# (ix +# 6#)))
                                        (f (I# (ix +# 7#)))
                 else   push8 Pull1

        return $ Flow getSize get1 get8
{-# INLINE [1] generate #-}


-- | Produce an flow of the given length with the same value in each position.
replicate :: Int -> a -> IO (Flow a)
replicate n x
        = generate n (const x)
{-# INLINE [1] replicate #-}


-- | Segmented replicate,
--   reading segment lengths and elements from unboxed vectors.
replicatesUnboxed
        :: U.Unbox a
        => Int
        -> U.Vector Int
        -> U.Vector a
        -> IO (Flow a)

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
--   TODO: only check for flow end when the current segment is finished.
--   TODO: pulling a single element should also hint how many we can pull next time.
--   TODO: could update total count only when we finish a segment.
replicatesDirect 
        :: Int                  -- Total length of result.
        -> (Int# -> Int#)       -- SegmentId -> Segment Length.
        -> (Int# -> a)          -- SegmentId -> Value to emit for this segment.
        -> IO (Flow a)

replicatesDirect (I# resultLen) getSegLen getValue
 = do   
        -- Local state.
        let !sCount     = 0     -- How many elements we've emitted so far.
        let !sSeg       = 1     -- Id of current segment.
        let !sSegLen    = 2     -- Length of current segment.
        let !sRemain    = 3     -- Elements remaining to emit in this segment.
        refState        <- UM.unsafeNew 4
        UM.unsafeWrite refState sCount  (0  :: Int)
        UM.unsafeWrite refState sSeg    (-1 :: Int)
        UM.unsafeWrite refState sSegLen (0  :: Int)
        UM.unsafeWrite refState sRemain (0  :: Int)

        let 
         getSize
          = do  !(I# count)     <- UM.unsafeRead refState sCount
                !(I# remain)    <- UM.unsafeRead refState sRemain
                return  $ Exact ((resultLen -# count) -# remain)



         get1 push1
          = do  !(I# seg)       <- UM.unsafeRead refState sSeg
                !(I# remain)    <- UM.unsafeRead refState sRemain
                result seg remain
          where 
                result seg remain
                 = do   
                        -- Check if there are any more elements to emit for this segment.
                        if remain ># 0#
                         then do
                                -- Emit a result from this segment.
                                UM.unsafeWrite refState sRemain (I# (remain -# 1#))
                                push1 $ Yield1 (getValue seg) (remain >=# 9#)

                         else result_doneSeg seg

                -- Advance to the next segment.
                result_doneSeg seg
                 = do   
                        -- Add the completed segment to the total count.
                        !(I# count)     <- UM.unsafeRead refState sCount
                        !(I# segLen)    <- UM.unsafeRead refState sSegLen
                        let !count'     = count +# segLen
                        UM.unsafeWrite refState sCount (I# count')

                        -- Check if we've finished the whole flow.
                        if count' >=# resultLen
                         then push1 Done
                         else result_nextSeg (seg +# 1#)

                -- Find the next segment with a non-zero length.
                result_nextSeg seg 
                 = do   let !segLen = getSegLen seg
                        if segLen ># 0#
                         then do
                                UM.unsafeWrite refState sSeg    (I# seg)
                                UM.unsafeWrite refState sSegLen (I# segLen)
                                result seg segLen

                         else result_nextSeg (seg +# 1#)


         get8 push8
          = refState `seq` result
          where 
                -- If we're far enough from the segment end then emit
                -- a packet of elements, otherwise tell the consumer
                -- they need to pull a single element at a time.
                result
                 = do   !(I# remain)    <- UM.unsafeRead refState sRemain
                        if remain >=# 8#
                         then   result_fromSeg remain
                         else   push8 Pull1

                -- Emit a packet of elements.
                result_fromSeg remain
                 = do
                        UM.unsafeWrite refState sRemain (I# (remain -# 8#))
                        !(I# seg)       <- UM.unsafeRead refState sSeg
                        let !x  = getValue seg
                        push8 $ Yield8 x x x x x x x x

        return $ Flow getSize get1 get8
{-# INLINE [1] replicatesDirect #-}


-------------------------------------------------------------------------------
-- | Yield a vector of the given length containing values @x@, @x+1@ etc.
enumFromN :: (U.Unbox a, Num a, Show a) => a -> Int -> IO (Flow a)
enumFromN start (I# len)
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 (I# len)

        refAcc   <- UM.unsafeNew 1
        UM.unsafeWrite refAcc   0 start

        let
         getSize
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                return  $ Exact count

         get1 push1
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count ># 0#
                 then do 
                        UM.unsafeWrite refCount 0 (I# (count -# 1#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 1)

                        push1 $ Yield1 acc (count >=# 9#)

                 else   push1 Done

         get8 push8
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count >=# 8#
                 then do
                        UM.unsafeWrite refCount 0 (I# (count -# 8#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 8)

                        push8 $ Yield8  (acc + 0) (acc + 1) (acc + 2) (acc + 3)
                                        (acc + 4) (acc + 5) (acc + 6) (acc + 7)
                 else   push8 Pull1

        return $ Flow getSize get1 get8
{-# INLINE [1] enumFromN #-}


