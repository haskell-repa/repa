
module Data.Array.Repa.Flow.Generate
        ( generate
        , replicate
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
         getSize _
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                return  $ Exact (len -# ix)

         get1 push1
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                if ix >=# len
                 then    push1 Nothing
                 else do UM.unsafeWrite refCount 0 (I# (ix +# 1#))
                         push1 $ Just (f (I# ix))

         get8 push8
          = do  !(I# ix) <- UM.unsafeRead refCount 0
                if ix +# 8# ># len
                 then   push8 $ Right 1
                 else do
                        UM.unsafeWrite refCount 0 (I# (ix +# 8#))
                        push8 $ Left    ( f (I# (ix +# 0#))
                                        , f (I# (ix +# 1#))
                                        , f (I# (ix +# 2#))
                                        , f (I# (ix +# 3#))
                                        , f (I# (ix +# 4#))
                                        , f (I# (ix +# 5#))
                                        , f (I# (ix +# 6#))
                                        , f (I# (ix +# 7#)))

        return $ Flow getSize get1 get8
{-# INLINE [1] generate #-}


-- | Produce an flow of the given length with the same value in each position.
replicate :: Int -> a -> IO (Flow a)
replicate n x
        = generate n (const x)
{-# INLINE [1] replicate #-}


-- | Yield a vector of the given length containing values @x@, @x+1@ etc.
enumFromN :: (U.Unbox a, Num a, Show a) => a -> Int -> IO (Flow a)
enumFromN start (I# len)
 = do   refCount <- UM.unsafeNew 1
        UM.unsafeWrite refCount 0 (I# len)

        refAcc   <- UM.unsafeNew 1
        UM.unsafeWrite refAcc   0 start

        let
         getSize _
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                return  $ Exact (len -# count)

         get1 push1
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count ==# 0#
                 then   push1 Nothing
                 else do 
                        UM.unsafeWrite refCount 0 (I# (count -# 1#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 1)

                        push1 $ Just acc

         get8 push8
          = do  !(I# count)     <- UM.unsafeRead refCount 0
                if count <=# 8#
                 then   push8 (Right 1)
                 else do
                        UM.unsafeWrite refCount 0 (I# (count -# 8#))

                        acc     <- UM.unsafeRead refAcc 0
                        UM.unsafeWrite refAcc   0 (acc + 8)

                        push8 $ Left    ( acc
                                        , acc + 1
                                        , acc + 2
                                        , acc + 3
                                        , acc + 4
                                        , acc + 5
                                        , acc + 6
                                        , acc + 7)

        return $ Flow getSize get1 get8
{-# INLINE [1] enumFromN #-}


-- | Segmented replicate,
--   where we have functions that produce segment lengths and elements
--   directly.
replicatesDirect 
        :: Int#                 -- Total length of result.
        -> (Int# -> Int#)       -- SegmentId -> Segment Length.
        -> (Int# -> a)          -- SegmentId -> Value to emit for this segment.
        -> IO (Flow a)

replicatesDirect resultLen getSegLen getValue
 = do   
        -- Local state.
        let !sCount     = 0     -- How many elements we've emitted so far.
        let !sSeg       = 1     -- Id of current segment.
        let !sRemain    = 2     -- Elements remaining to emit in this segment.
        refState        <- UM.unsafeNew 3
        UM.unsafeWrite refState 0 (0 :: Int)
        UM.unsafeWrite refState 1 (0 :: Int)
        UM.unsafeWrite refState 2 (0 :: Int)

        let 
         getSize _
          = do  !(I# count) <- UM.unsafeRead refState sCount
                return  $ Exact (resultLen -# count)

         get1 push1
          = refState `seq` result
          where 
                result
                 = do   !(I# count)     <- UM.unsafeRead refState sCount
                        if count >=# resultLen
                         then push1 Nothing                    -- No more elements in flow.
                         else result_nextElem count            -- Emit an element.
                           
                -- Emit an element.     
                result_nextElem count
                 = do   !(I# seg)       <- UM.unsafeRead refState sSeg
                        !(I# remain)    <- UM.unsafeRead refState sRemain
                        if remain <=# 0#                       -- Any more elems for this segment?
                         then result_nextSeg count (seg +# 1#) --  no:  Advance to the next segment.
                         else result_fromSeg count seg remain  --  yes: Emit from this segment. 

                 -- Advance to the next non-zero length segment.
                result_nextSeg count seg
                 = do   let !segLen     = getSegLen seg 
                        if segLen ># 0#
                         then result_fromSeg count seg segLen
                         else result_nextSeg count (seg +# 1#)

                -- Emit a result from this segment.
                result_fromSeg count seg remain
                 = do   
                        -- Update state.
                        UM.unsafeWrite refState sCount  (I# (count  +# 1#))
                        UM.unsafeWrite refState sSeg    (I# seg)
                        UM.unsafeWrite refState sRemain (I# (remain -# 1#))

                        push1 $ Just (getValue seg)

         get8 push8
          = push8 $ Right 1

        return $ Flow getSize get1 get8
{-# INLINE replicatesDirect #-}
