
module Data.Array.Repa.Bulk.Par.Interleaved
        (fillInterleaved)
where
import Data.Array.Repa.Bulk.Par.Gang
import GHC.Exts


-- | Fill something in parallel.
-- 
--   * Threads handle elements in row major, round-robin order.
--
--   * Using this method helps even out unbalanced workloads.
--
fillInterleaved
        :: Gang                 -- ^ Gang to run the operation on.
        -> (Int# -> a -> IO ()) -- ^ Update function to write into result buffer.
        -> (Int# -> a)          -- ^ Function to get the value at a given index.
        -> Int#                 -- ^ Number of elements.
        -> IO ()

fillInterleaved gang write getElem len 
 = gangIO gang
 $  \thread -> 
    let !step    = threads
        !start   = thread
        !count   = elemsForThread thread
    in  fill step start count

 where
        -- Decide now to split the work across the threads.
        !threads        = gangSize gang

        -- All threads get this many elements.
        !chunkLenBase   = len `quotInt#` threads

        -- Leftover elements to divide between first few threads.
        !chunkLenSlack  = len `remInt#`  threads

        -- How many elements to compute with this thread.
        elemsForThread thread
         | thread <# chunkLenSlack = chunkLenBase +# 1#
         | otherwise               = chunkLenBase
        {-# INLINE elemsForThread #-}

        -- Evaluate the elements of a single chunk.
        fill !step !ix0 !count0
         = go ix0 count0
         where
          go !ix !count
             | count <=# 0# = return ()
             | otherwise
             = do write ix (getElem ix)
                  go (ix +# step) (count -# 1#)
        {-# INLINE fill #-}
{-# INLINE [0] fillInterleaved #-}
