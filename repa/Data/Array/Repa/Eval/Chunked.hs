{-# LANGUAGE MagicHash #-}
-- | Evaluate an array by breaking it up into linear chunks and filling
--   each chunk in parallel.
module Data.Array.Repa.Eval.Chunked
        ( fillLinearS
        , fillBlock2S
        , fillChunkedP
        , fillChunkedIOP)
where
import Data.Array.Repa.Index
import Data.Array.Repa.Eval.Gang

import GHC.Exts
import Prelude          as P

-------------------------------------------------------------------------------
-- | Fill something sequentially.
-- 
--   * The array is filled linearly from start to finish.  
-- 
fillLinearS
        :: Int                  -- ^ Number of elements.
        -> (Int -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> (Int -> a)           -- ^ Fn to get the value at a given index.
        -> IO ()

fillLinearS !(I# len) write getElem
 = fill 0#
 where  fill !ix
         | 1# <- ix >=# len
         = return ()

         | otherwise
         = do   write (I# ix) (getElem (I# ix))
                fill (ix +# 1#)
{-# INLINE [0] fillLinearS #-}


-------------------------------------------------------------------------------
-- | Fill a block in a rank-2 array, sequentially.
--
--   * Blockwise filling can be more cache-efficient than linear filling for
--     rank-2 arrays.
--
--   * The block is filled in row major order from top to bottom.
--
fillBlock2S
        :: (Int  -> a -> IO ()) -- ^ Update function to write into result buffer.
        -> (DIM2 -> a)          -- ^ Fn to get the value at the given index.
        -> Int#                 -- ^ Width of the whole array.
        -> Int#                 -- ^ x0 lower left corner of block to fill.
        -> Int#                 -- ^ y0
        -> Int#                 -- ^ w0 width of block to fill
        -> Int#                 -- ^ h0 height of block to fill
        -> IO ()

fillBlock2S
        write getElem
        !imageWidth !x0 !y0 !w0 h0

 = do   fillBlock y0 ix0
 where  !x1     = x0 +# w0
        !y1     = y0 +# h0
        !ix0    = x0 +# (y0 *# imageWidth)

        {-# INLINE fillBlock #-}
        fillBlock !y !ix
         | 1# <- y >=# y1
         = return ()

         | otherwise
         = do   fillLine1 x0 ix
                fillBlock (y +# 1#) (ix +# imageWidth)

         where  {-# INLINE fillLine1 #-}
                fillLine1 !x !ix'
                 | 1# <- x >=# x1
                 = return ()

                 | otherwise
                 = do   write (I# ix') (getElem (Z :. (I# y) :. (I# x)))
                        fillLine1 (x +# 1#) (ix' +# 1#)

{-# INLINE [0] fillBlock2S #-}


-------------------------------------------------------------------------------
-- | Fill something in parallel.
-- 
--   * The array is split into linear chunks,
--     and each thread linearly fills one chunk.
-- 
fillChunkedP
        :: Int                  -- ^ Number of elements.
        -> (Int -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> (Int -> a)           -- ^ Fn to get the value at a given index.
        -> IO ()

fillChunkedP !(I# len) write getElem
 =      gangIO theGang
         $  \(I# thread) -> 
              let !start   = splitIx thread
                  !end     = splitIx (thread +# 1#)
              in  fill start end

 where
        -- Decide now to split the work across the threads.
        -- If the length of the vector doesn't divide evenly among the threads,
        -- then the first few get an extra element.
        !(I# threads)   = gangSize theGang
        !chunkLen       = len `quotInt#` threads
        !chunkLeftover  = len `remInt#`  threads

        {-# INLINE splitIx #-}
        splitIx thread
         | 1# <- thread <# chunkLeftover 
         = thread *# (chunkLen +# 1#)

         | otherwise    
         = thread *# chunkLen  +# chunkLeftover

        -- Evaluate the elements of a single chunk.
        {-# INLINE fill #-}
        fill !ix !end
         | 1# <- ix >=# end     
         = return ()

         | otherwise
         = do   write (I# ix) (getElem (I# ix))
                fill (ix +# 1#) end
{-# INLINE [0] fillChunkedP #-}


-------------------------------------------------------------------------------
-- | Fill something in parallel, using a separate IO action for each thread.
--
--   * The array is split into linear chunks,
--     and each thread linearly fills one chunk.
--
fillChunkedIOP
        :: Int  -- ^ Number of elements.
        -> (Int -> a -> IO ())          
                -- ^ Update fn to write into result buffer.
        -> (Int -> IO (Int -> IO a))    
                -- ^ Create a fn to get the value at a given index.
                --   The first `Int` is the thread number, so you can do some
                --   per-thread initialisation.
        -> IO ()

fillChunkedIOP !(I# len) write mkGetElem
 =      gangIO theGang
         $  \(I# thread) -> 
              let !start = splitIx thread
                  !end   = splitIx (thread +# 1#)
              in fillChunk thread start end 

 where
        -- Decide now to split the work across the threads.
        -- If the length of the vector doesn't divide evenly among the threads,
        -- then the first few get an extra element.
        !(I# threads)   = gangSize theGang
        !chunkLen       = len `quotInt#` threads
        !chunkLeftover  = len `remInt#`  threads

        {-# INLINE splitIx #-}
        splitIx thread
         | 1# <- thread <# chunkLeftover = thread *# (chunkLen +# 1#)
         | otherwise                     = thread *# chunkLen  +# chunkLeftover

        -- Given the threadId, starting and ending indices. 
        --      Make a function to get each element for this chunk
        --      and call it for every index.
        {-# INLINE fillChunk #-}
        fillChunk !thread !ixStart !ixEnd
         = do   getElem <- mkGetElem (I# thread)
                fill getElem ixStart ixEnd
                
        -- Call the provided getElem function for every element
        --      in a chunk, and feed the result to the write function.
        {-# INLINE fill #-}
        fill !getElem !ix0 !end
         = go ix0 
         where  go !ix
                 | 1# <- ix >=# end
                 = return ()

                 | otherwise
                 = do   x       <- getElem (I# ix)
                        write (I# ix) x
                        go (ix +# 1#)
{-# INLINE [0] fillChunkedIOP #-}


