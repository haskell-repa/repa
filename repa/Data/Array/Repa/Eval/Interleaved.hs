{-# LANGUAGE MagicHash #-}
-- | Evaluate an array in parallel in an interleaved fashion,
--  with each by having each processor computing alternate elements.
module Data.Array.Repa.Eval.Interleaved
        ( fillInterleavedP)
where
import Data.Array.Repa.Eval.Gang
import GHC.Exts
import Prelude          as P


-- | Fill something in parallel.
-- 
--   * The array is split into linear chunks and each thread fills one chunk.
-- 
fillInterleavedP
        :: Int                  -- ^ Number of elements.
        -> (Int -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> (Int -> a)           -- ^ Fn to get the value at a given index.
        -> IO ()

{-# INLINE [0] fillInterleavedP #-}
fillInterleavedP !(I# len) write getElem
 =      gangIO theGang
         $  \(I# thread) -> 
              let !step    = threads
                  !start   = thread
                  !count   = elemsForThread thread
              in  fill step start count

 where
        -- Decide now to split the work across the threads.
        !(I# threads)   = gangSize theGang

        -- All threads get this many elements.
        !chunkLenBase   = len `quotInt#` threads

        -- Leftover elements to divide between first few threads.
        !chunkLenSlack  = len `remInt#`  threads

        -- How many elements to compute with this thread.
        elemsForThread thread
         | 1# <- thread <# chunkLenSlack
         = chunkLenBase +# 1#

         | otherwise
         = chunkLenBase
        {-# INLINE elemsForThread #-}

        -- Evaluate the elements of a single chunk.
        fill !step !ix0 !count0
         = go ix0 count0
         where
          go !ix !count
             | 1# <- count <=# 0# 
             = return ()

             | otherwise
             = do write (I# ix) (getElem (I# ix))
                  go (ix +# step) (count -# 1#)
        {-# INLINE fill #-}
